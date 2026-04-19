(** Visual demo for [Twig.Layout].

    Run:        dune exec twig-layout-demo
    Quit:       q  or Ctrl-C

    Keys:
      s           split focused pane horizontally (new pane below)
      v           split focused pane vertically   (new pane right)
      c           close focused pane
      h/j/k/l     move focus left/down/up/right
      arrow keys  same as hjkl
      <  or  ,    shrink focused pane by 5%% of parent
      >  or  .    grow focused pane by 5%% of parent
      =           equalize all splits to 50/50

    Focused pane's title bar is bolded and inverse-colored
    per [Theme.default.chrome.title_focused]. *)

open Twig

let pane_counter = ref 0

let next_pane_title () =
	incr pane_counter;
	Printf.sprintf "pane-%d" !pane_counter

let demo_content title =
	Doc.of_string
		(Printf.sprintf
			 "%s\n\n\
			  This is a twig.Layout demo pane.\n\
			  \n\
			  Commands:\n\
			  \  s           split horizontal (below)\n\
			  \  v           split vertical   (right)\n\
			  \  c           close this pane\n\
			  \  h j k l     move focus\n\
			  \  < >         resize focused pane\n\
			  \  =           equalize splits\n\
			  \  q           quit\n\
			  \n\
			  Your focus starts on this pane.\n"
			 title)

let make_pane () =
	let title = next_pane_title () in
	let buf = Buf.of_doc (demo_content title) in
	(title, buf)

type state = {
	layout : Layout.t;
	rect   : Rect.t;
	quit   : bool;
}

let get_rect () =
	let rows, cols = Terminal.get_size () in
	Rect.make ~row:0 ~col:0 ~rows ~cols

let initial_state () =
	let title, buf = make_pane () in
	let layout = Layout.single ~title buf () in
	{ layout; rect = get_rect (); quit = false }

let split dir s =
	let title, buf = make_pane () in
	{ s with layout = Layout.split s.layout dir ~title buf () }

let focus_move dir s =
	{ s with layout = Layout.focus_move s.layout ~rect:s.rect dir }

let resize delta s =
	{ s with layout = Layout.resize s.layout ~delta }

let close s =
	match Layout.close s.layout with
	| Some l -> { s with layout = l }
	| None -> s

let equalize s =
	{ s with layout = Layout.equalize s.layout }

let on_input (event : Input.event) (s : state) : state =
	match event with
	| Resize -> { s with rect = get_rect () }
	| Eof | Ctrl 'c' -> { s with quit = true }
	| Arrow Left -> focus_move `Left s
	| Arrow Right -> focus_move `Right s
	| Arrow Up -> focus_move `Up s
	| Arrow Down -> focus_move `Down s
	| Char u ->
			if Uchar.is_char u then
				match Uchar.to_char u with
				| 'q' -> { s with quit = true }
				| 's' -> split Horizontal s
				| 'v' -> split Vertical s
				| 'c' -> close s
				| '=' -> equalize s
				| '<' | ',' -> resize (-0.05) s
				| '>' | '.' -> resize 0.05 s
				| 'h' -> focus_move `Left s
				| 'j' -> focus_move `Down s
				| 'k' -> focus_move `Up s
				| 'l' -> focus_move `Right s
				| _ -> s
			else s
	| _ -> s

let render (s : state) : string =
	(* Clear screen + home cursor before drawing the layout so
	   we start from a known state each frame. Layout.render
	   emits its own CSI move escapes per pane. *)
	"\x1b[2J\x1b[H"
	^ Layout.render s.layout ~rect:s.rect ~theme:Theme.default

let () =
	let init = initial_state () in
	Terminal.install_resize_handler ();
	let saved = Terminal.enter_raw_mode () in
	Terminal.enable_kitty_keyboard ();
	(* Hide cursor; our layout doesn't draw a cursor. *)
	Terminal.write "\x1b[?25l";
	let cleanup () =
		Terminal.disable_kitty_keyboard ();
		(* Show cursor + clear screen + home. *)
		Terminal.write "\x1b[?25h\x1b[2J\x1b[H";
		Terminal.restore saved
	in
	let config : state Loop.config = {
		on_input;
		on_update = (fun s -> s);
		render;
		should_quit = (fun s -> s.quit);
	} in
	try
		Eio_main.run (fun _env ->
			ignore (Loop.run ~config ~init ()));
		cleanup ()
	with e ->
		cleanup ();
		raise e
