(* Graphic functions for graphic mode display *)

(* repr_graph: hash table of the visual representation of the characters in graphic mode *)
let repr_graph = visualgraph ();;

(* gscreen: content of the graphic screen *)
let gscreen = Array.make_matrix 64 128 false;;

(** Graphic display **)
let gdraw (ren : Sdlrender.t) : unit =
  print_mat ren gscreen false
    (fun ren -> rect ren (!margin_h-1) (!margin_v-1) (!width+1) (!height+1));;

(** Graphic functions **)

(* Converts the coordinates x,y, in the system {xmin, xmax, ymin, ymax}, to a pixel in {1, 127, 1, 63} *)
(* First draft *)
let rescale (p : parameters) (x : float) (y : float) : int * int =
  let ax = (x -. p.xmin) /. (p.xmax -. p.xmin) in
  let ay = (y -. p.ymin) /. (p.ymax -. p.ymin) in
  (1 + true_int_of_float (ax *. 127),
   1 + true_int_of_float (ay *. 63));;

(* Draws a horizontal line in the current system of coordinates at ordinate y *)
let draw_horizontal_line (ren : Sdlrender.t) (p : parameters) (y : float) : unit =
  let (_, j) = rescale p 0. y in
  horizontal_line ren gscreen 1 127 j;;

(* Draws a vertical line in the current system of coordinates at abscissa x *)
let draw_horizontal_line (ren : Sdlrender.t) (p : parameters) (x : float) : unit =
  let (i, _) = rescale p x 0. in
  vertical_line ren gscreen 1 63 i;;

(* Combines horizontal and vertical line drawings, but more efficient *)
(* Draws the scales (one plot per xstep, ystep on each axis) *)
let draw_axes (ren : Sdlrender.t) (p : parameters) : unit =
  let (i, j) = rescale p 0. 0. in
  horizontal_line ren gscreen 1 127 j;
  vertical_line ren gscreen 1 63 i;
  (* Scales *)
  if not (is_zero_float p.xstep) then
    let nb_step_xplus  = true_int_of_float (p.xmax /. p.xstep) in
    let nb_step_xminus = true_int_of_float (p.xmin /. p.xstep) in
    let step_xplus = Array.init nb_step_xplus (fun i -> (float_of_int i)*.p.xstep) in
    let step_xminus = Array.init nb_step_xminus (fun i -> (float_of_int (-i))*.p.xstep) in
    (Array.iter (fun x -> let (i,j) = rescale p x 0. in ploton ren i (j+1)) step_xplus;
    Array.iter (fun x -> let (i,j) = rescale p x 0. in ploton ren i (j+1)) step_xminus);
  if not (is_zero_float p.ystep) then
    let nb_step_yplus  = true_int_of_float (p.ymax /. p.ystep) in
    let nb_step_yminus = true_int_of_float (p.ymin /. p.ystep) in
    let step_yplus = Array.init nb_step_yplus (fun i -> (float_of_int i)*.p.ystep) in
    let step_yminus = Array.init nb_step_yminus (fun i -> (float_of_int (-i))*.p.ystep) in
    (Array.iter (fun y -> let (i,j) = rescale p 0. y in ploton ren (i+1) y) step_yplus;
    Array.iter (fun y -> let (i,j) = rescale p 0. y in ploton ren (i+1) y) step_yminus);;

(* Draws the window in the current coordinates system *)
let draw_window (ren : Sdlrender.t) (p : parameters) : unit =
  wipe gscreen;
  if p.axeson then
    draw_axes ren p;;


(* Auxiliary function to draw_pict *)
(* Adds to rects the rectangles needed to draw line j of the picture stored in matrix m,
  between the abscissas imin and imax included *)
let aux_draw_line (m : bool array array) (j : int) (imin : int) (imax : int) (rects : Sdlrect.t list ref) : unit =
  let ibeg = ref (-1) in
  for i = imin to imax do
    if m.(j).(i) then
      (gscreen.(j).(i) <- true;
      if !ibeg = (-1) then
        ibeg := i)
    else
      if !ibeg <> -1 then
        (rects :=
          (Sdlrect.make2
            ~pos:(!margin_h + !size * !ibeg, !margin_v + !size * j)
            ~dims:((i - 1 - !ibeg) * !size, !size))
          ::!rects;
        !ibeg := -1)
  done;
  if !ibeg <> -1 then
    rects :=
      (Sdlrect.make2
        ~pos:(!margin_h + !size * !ibeg, !margin_v + !size * j)
        ~dims:((imax - 1 - !ibeg) * !size, !size))
      ::!rects;;

(* Draws the picture pict (0..19) on screen *)
(* The first given number of bytes is drawn:
  8 pixels per bytes, 16 bytes per line,
  1024 = normal picture, < 16 = less than one line *)
let draw_pict (ren : Sdlrender.t) (p : parameters) (pict : int) (wanted_size : int) : unit =
  let (pict_size, m) = p.pict.(pict) in
  let actual_size = min 1024 (min pict_size wanted_size) in
  let rects = ref [] in
  let nb_lines = actual_size / 16 in
  let ibeg = ref 0 in
	let i = ref 0 in
  for j = 0 to nb_lines - 1 do
    aux_draw_line m j 0 127 rects
  done;
  let remainder = actual_size mod 16 in
  aux_draw_line m nb_lines 0 (remainder-1) rects;
  Sdlrender.fill_rects ren (Array.of_list !rects);;