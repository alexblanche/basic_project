(* Parameters and updating functions for graphic display *)

(** SDL2 initialization **)

let sdl_init () : unit =
  Sdl.init [`VIDEO; `EVENTS];
  Sdlttf.init ()
in
sdl_init ();;

let sdl_quit () : unit =
  Sdl.quit ();
  Sdlttf.quit ();;

(* Exception raised when the window is closed *)
exception Window_Closed;;

(** Parameters **)

(* Window parameters *)
let margin_h = ref (-1);; (* Horizontal margin between the graphics window and the calculator screen *)
let margin_v = ref (-1);; (* Vertical margin between the graphics window and the calculator screen *)
(* The margins will be updated with update_parameters below. *)
let size = ref 7;; (* Size of a pixel of the screen *)
let width = ref (128 * !size);; (* Width of the calculator screen (without the margin) *)
let height = ref (64 * !size);; (* Height of the calculator screen (without the margin) *)

(* Number of window resizings after which the screen is redrawn *)
let resize_threshold = 15;;

(* Font used for draw_string *)
let font = Sdlttf.open_font ~file:"data/UbuntuMono-R.ttf" ~ptsize:16;;


(** Updating functions **)

(* Indicates that the graphic parameters were updated since the screen was last drawn *)
let parameters_updated = ref false;;

(* When the window is resized, updates margin, size, width and height *)
let update_parameters (new_width : int) (new_height : int) : unit =
  if 100*new_width >= 185*new_height
    then size := int_of_float (0.01326 *. float_of_int new_height)
    else size := int_of_float (0.00718 *. float_of_int new_width);
  width := 128 * !size;
  height := 64 * !size;
  margin_h := (new_width - !width)/2;
  margin_v := (new_height - !height)/2;
  parameters_updated := true;;

(* Parameters initialization *)
update_parameters (!width+2*40) (!height+2*40);;