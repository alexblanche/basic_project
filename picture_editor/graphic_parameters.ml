(* Parameters and updating functions for graphic display *)

(** Parameters **)

(* Constants *)
let margin_h = ref 40;; (* Horizontal margin between the graphics window and the calculator screen *)
let margin_v = ref 40;; (* Vertical margin between the graphics window and the calculator screen *)
let size = ref 7;; (* Size of a pixel of the screen *)
let width = ref (128 * !size);; (* Width of the calculator screen (without the margin) *)
let height = ref (64 * !size);; (* Height of the calculator screen (without the margin) *)

(* Colors *)
let white = (255, 255, 255);;
let gray = (120, 120, 148);;
let black = (0, 0, 0);;


(** Updating functions **)

(* When the window is resized, updates margin, size, width and height *)
let update_parameters (new_width : int) (new_height : int) : unit =
  if 100*new_width >= 185*new_height
    then size := int_of_float (0.01326 *. float_of_int new_height)
    else size := int_of_float (0.00718 *. float_of_int new_width);
  width := 128 * !size;
  height := 64 * !size;
  margin_h := (new_width - !width)/2;
  margin_v := (new_height - !height)/2;;




(* Test function: draws a red rectangle in place of the calculator screen, and adapts to window resizing *)
let test () =
  Sdl.init [`VIDEO];
	let window = Sdlwindow.create ~title:"Display test" ~pos:(`pos 300, `pos 100) ~dims:(1000, 800) ~flags:[] in
	
	let renderer = Sdlrender.create_renderer ~win:window ~index:0 ~flags:[] in
	Sdlrender.set_draw_blend_mode renderer SdlblendMode.BNone;

  let x = ref 1000 in
  let y = ref 800 in
  let cpt_resize = ref 0 in
  let threshold = 15 in

  let print_rect () =
    update_parameters !x !y;
    Sdlrender.set_draw_color renderer ~rgb:white ~a:255;
    Sdlrender.clear renderer;
    let r = Sdlrect.make ~pos:(!margin_h, !margin_v) ~dims:(128 * !size, 64 * !size) in
    Sdlrender.set_draw_color renderer ~rgb:(255,50,50) ~a:255;
    Sdlrender.fill_rect renderer r;
    Sdlrender.render_present renderer
  in
  
  let rec aux () =
		match Sdlevent.poll_event () with
			| Some (Window_Event {kind = WindowEvent_Close})
			| Some KeyDown {keycode = Escape} -> Sdl.quit ()

      | Some (Window_Event {kind = WindowEvent_Resized wxy}) ->
        (x := wxy.win_x;
        y := wxy.win_y;
        incr cpt_resize;
        if !cpt_resize >= threshold
          then (print_rect (); cpt_resize := 0);
        aux ())

      | Some (Window_Event {kind = WindowEvent_Exposed})
      | Some Keymap_Changed ->
        (cpt_resize := threshold;
        aux ())

      | _ -> aux ()
	in
	
	try
    print_rect ();
		aux ()
	with
		| Failure _ -> Sdl.quit ();;