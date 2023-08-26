(* Graphic functions *)
(* To do *)

#use "topfind"
#require "graphics"

#use "picture_editor/picture_drawer.ml"
#use "basic_parsing/basic_encoding.ml"

let open_graphic () : unit =
  open_graph "";
	resize_window (2*margin + width) (2*margin + height);
	set_window_title "Basic Emulator";
	cache ();
	sync();;

let disp () : unit =
  let b = ref false in
  while not !b do
    let c = read_key () in
    if c = '\013' (* Enter *)
      then b := true
  done;;

(* Next: text display, Locate, string display (with scrolling) *)

