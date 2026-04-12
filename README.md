# twig

A terminal text editor written in OCaml. Grapheme-aware, with soft wrap, syntax highlighting, a two-tier command system, and an immutable AVL-tree document model.

## Features

- **Grapheme-aware editing** via uucp/uutf/uuseg (UAX #29). Cursor never lands mid-cluster; ZWJ emoji, combining marks, and CJK wide characters are handled correctly.
- **Immutable AVL-tree document** with O(log n) get/insert/delete/replace and cached size and byte sums. Persistence gives undo/redo nearly for free.
- **Soft wrap** (Alt-Z to toggle). Long lines break into wrapped screen rows; arrow keys navigate through segments, preserving display column.
- **Syntax highlighting** for C, OCaml, Dune, and Opam with cross-line state tracking (multi-line comments, nested OCaml `(* *)` comments).
- **Two-tier command system**. Shift+Space or ESC enters chord mode with a discoverable legend in the status bar. Single keypress fires common actions (save, quit, find, goto, theme, etc.). Press `:` for a full text prompt for complex commands.
- **Preview-first goto**: as you type `goto 42`, the target line highlights before you commit.
- **Kitty keyboard protocol** (flags 1+8+16) for reliable Shift+Space, Ctrl, and Alt decoding with text-as-codepoints for correct shifted symbols on any keyboard layout.
- **Selection** with Shift-arrow, Ctrl-G mark, copy/cut/paste, and OSC 52 clipboard integration.
- **Incremental search** (Ctrl-F) with live match highlighting across the visible viewport.
- **Find and replace** via `:replace <old> with <new>`.
- **Auto-indent** on Enter. Block indent/outdent with Tab/Shift-Tab on selections.
- **Undo/redo** with keystroke burst grouping.
- **4 built-in themes**: default, nightwatch, solar, mono. Cycle with `t` in command mode or `:theme <name>`.
- **Line numbers** (Alt-L to toggle), tab expansion (8-column stops), atomic save, open-file prompt (Ctrl-O), SIGWINCH-responsive resize.
- **159 tests** covering Doc, Grapheme, Command, State, and Syntax.

## Building

Requires OCaml 5.x and dune 3.x.

```bash
# Install dependencies
opam install uutf uucp uuseg

# Build
dune build

# Run
dune exec bin/main.exe -- [file]

# Run tests
dune runtest
```

## Keybindings

### Editing
| Key | Action |
|-----|--------|
| Ctrl-S | Save |
| Ctrl-Z | Undo |
| Ctrl-Y | Redo |
| Ctrl-O | Open file |
| Ctrl-Q | Quit |
| Ctrl-F | Find |
| Ctrl-N | Next match |
| Ctrl-G | Set mark |
| Ctrl-C | Copy selection |
| Ctrl-X | Cut selection |
| Ctrl-V | Paste |
| Tab | Insert tab / indent block (with selection) |
| Shift-Tab | Outdent block |
| Ctrl-Left/Right | Word motion |
| Ctrl-Home/End | Document start/end |
| Shift-Arrow | Extend selection |
| Alt-Z | Toggle soft wrap |
| Alt-L | Toggle line numbers |

### Command mode (Shift+Space or ESC)
| Key | Action |
|-----|--------|
| s | Save |
| S | Save as |
| q | Quit |
| Q | Force quit |
| f | Find |
| r | Replace |
| g | Goto line (with preview) |
| t | Cycle theme |
| w | Toggle wrap |
| n | Toggle line numbers |
| u | Undo |
| U | Redo |
| d | Duplicate line |
| h | Help |
| : | Full command prompt |

### Text commands (via `:` prompt)
```
save / save as <path> / quit / q! / wq
goto <n> / g <n> / top / bottom
replace <old> with <new>
dup [n]
theme [name]
help
```

## Architecture

Single-library design (`lib/`) with a thin executable layer (`bin/main.ml`).

| Module | Purpose |
|--------|---------|
| Doc | Immutable AVL tree of lines with grapheme-indexed insert/delete/replace, substring search, and range extraction |
| State | Editor state: doc, cursor, undo/redo stacks with burst grouping, modes (Edit/Searching/Opening_file/Command_chord/Command_prompt), mark, yank, theme |
| Command | Explicit ADT for all editor actions, applied through State.apply |
| Render | Viewport, soft wrap, segment-aware cursor positioning, syntax coloring with selection/match/preview overlays |
| Grapheme | UAX #29 cluster boundaries, byte/index conversion, display width, word boundaries |
| Syntax | Line-based tokenizers for C, OCaml, Dune, Opam with cross-line comment state |
| Theme | Color schemes (default, nightwatch, solar, mono) |
| Input | Terminal input decoding: kitty keyboard protocol CSI u, escape sequences, UTF-8 |
| Terminal | Raw mode, SIGWINCH, kitty protocol enable/disable |
| Ui | Viewport dimensions, wrap/line-number toggles |
| Clipboard | Base64 encoder and OSC 52 emitter |
| Position | Shared (line, column) coordinate type |

## License

MIT
