# twig

Terminal buffer engine in OCaml. An embeddable library for building
TUI applications: grapheme-aware text storage, splittable pane
layout, markdown rendering, input decoding, and a minimal event
loop. The editor that originally lived here has been split out to
[twig-edit](https://github.com/deths74r/twig-edit).

Downstream consumers:
- [twig-edit](https://github.com/deths74r/twig-edit) — the editor.
- [lmi](https://github.com/deths74r/lmi) — persistent-memory
  substrate for LLMs; uses twig for its TUI panes, layout, and
  streaming markdown rendering.

## What's in here

Library modules (`lib/`):

| Module | Purpose |
|--------|---------|
| `Doc` | Immutable AVL tree of lines with grapheme-indexed insert/delete/replace, substring search, and range extraction. |
| `Buf` | Reusable text buffer: `Doc` + cursor + selection + yank + undo/redo with keystroke burst grouping. The core engine type; applications wrap it with their own state. |
| `Grapheme` | UAX #29 cluster boundaries, byte/index conversion, display width, word boundaries. Built on `uucp` / `uuseg`. |
| `Rect` | Rectangle algebra for terminal layout (half-open cell coordinates, split/intersect). |
| `Viewport` | Visible window into a `Buf`: geometry, scroll, soft-wrap segmentation that respects display width. |
| `Layout` | Pane tree: binary splits (horizontal / vertical) with focus path, swap, fullscreen, resize, equalize, and render. Supports per-leaf `render_mode` (markdown or plain-with-prefix) and `min_rows`. |
| `Markdown` | Per-line stateful tokenizer with cross-line fenced-code state, used by `Layout.render` to style spans. |
| `Theme` | Structured terminal styles (chrome / markdown / syntax layers) with a TOML loader. |
| `Terminal` | Raw mode, SIGWINCH, region-scoped `with_clip`, capture helpers, kitty keyboard protocol. |
| `Input` | Terminal input decoding: kitty keyboard protocol CSI u, escape sequences, UTF-8, signaled wake (`Input.wake`). |
| `Loop` | Eio-based event loop (`input → update → render`) with input read in a systhread so the scheduler stays responsive. |
| `Clipboard` | Base64 encoder and OSC 52 emitter. |
| `Position` | Shared `(line, column)` coordinate type. |

Demo (`bin/layout_demo/`) — a minimal splittable workspace that
exercises the `Layout` API directly.

## Building

Requires OCaml ≥ 5.2 and dune ≥ 3.0.

```bash
opam install . --deps-only
dune build
dune runtest
dune install          # installs the library for downstream projects
dune exec bin/layout_demo/main.exe
```

## Using twig in another project

After `dune install`, depend on it from your `dune` file:

```
(libraries twig)
```

Typical setup — open the namespace, make a buffer, wrap it in a
single-leaf layout, run the event loop:

```ocaml
open Twig

let () =
  Eio_main.run @@ fun env ->
  let buf = Buf.empty in
  let state = ref (Layout.single buf ()) in
  let render s = ... in
  let on_input ev s = ... in
  Loop.run env { on_input; on_update = (fun s -> s); render } !state
```

Applications that need styled panes pass `~render_mode` to
`Layout.single` / `Layout.split`:

```ocaml
let prompt_pane =
  Layout.single
    ~render_mode:(Plain {
      prefix_first = "❯ ";
      prefix_rest  = "  ";
      prefix_style = Some theme.chrome.border_focused;
    })
    ~min_rows:2
    input_buf
    ()
```

## Architecture

twig is **role-blind**: the engine knows about buffers, viewports,
panes, and rendering, but nothing about what each pane *means*.
Applications layer role/semantics on top (e.g. LMI's `Role` sidecar
map keyed on `Layout.path`).

The data flow is one-way: application state → `Layout.render` →
terminal output. `Layout.render` returns the focused leaf's cursor
screen position (as `(int * int) option`) so the caller can emit
the terminal cursor-move sequence wherever it wants it.

## License

MIT
