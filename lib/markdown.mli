(** Per-line stateful markdown tokenizer.

    Rendering consumers (Layout's render path, LMI's Convo pane)
    call [tokenize_line] on each visible source line, threading
    [state] across lines. The tokenizer returns a list of spans
    that partitions the line by style — non-overlapping, sorted
    by byte offset, covering every byte of the line.

    State is MINIMAL: only [fenced_code] crosses line boundaries
    (an open ``` extends to the closing ```). Everything else
    is recognized per-line: a line beginning with `#` is a
    heading regardless of what came before; a line starting
    with `|` is a table row. This loses some fidelity versus a
    full AST parser (e.g., two blank lines between list items
    don't "end" the list) but handles the LLM output shape
    well and keeps the tokenizer streaming-friendly.

    Scope covers: headings (1-6), fenced code blocks with
    language tag, inline code, bold [**...**], italic [*...*]
    and [_..._], strikethrough [~~...~~], links [\[text\](url)],
    bullet and numbered list markers, blockquote prefix,
    horizontal rules, table rows. Explicitly NOT supported:
    reference-style links, footnotes, HTML-in-markdown,
    multi-line emphasis — see docs/design/18_tui.md §13. *)

type state = {
	fenced_code : string option;
		(** [Some lang] while inside a ``` block; the language
		    tag (may be "") lets the Convo pane later swap in
		    a language-specific syntax highlighter. *)
}

val init : state

type span = {
	start : int;   (** inclusive byte offset *)
	stop  : int;   (** exclusive byte offset *)
	style : Theme.style;
}

val tokenize_line :
	state:state ->
	line:string ->
	theme:Theme.markdown ->
	span list * state
(** [tokenize_line ~state ~line ~theme] produces the span list
    for [line] and the new state for the next line. The span
    list covers [line] from offset 0 to [String.length line]
    without gaps; spans with [style = Theme.plain] indicate
    "no specific styling here". *)
