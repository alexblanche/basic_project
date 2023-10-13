(* Graphic functions for graphic mode display *)

(* None of these function refresh the screen, this task is left to the calling program *)

(* Pictures are implemented in a simplified way for the moment:
  - In Casio, normal pictures take up 2048 bytes of space, and contain two superimposed screens
  that are printed one after the other with RclPict.
  By storing pictures normally (through StoPict), the second screen is always empty.
  - In the emulator, only one screen (or part of one screen) is stored, and is printed through RclPict.
  If the Picture is compressed, the following Pictures are printed until the number of bytes printed reach
  2048.
  
  This does not enable tweaking of Picture behavior (to my knowledge unseen as I type these lines), such as:
  - Pict 1 contains image A (1024 bytes), Pict 10 contains a blank screen (1024 bytes) and image B (1024 bytes),
    RclPict 1 draws image A, RclPict 10 draws image B, {Pict 1, 10} take up 3*1024 bytes, instead of 4*1024 normally *)

(* repr_graph: hash table of the visual representation of the characters in graphic mode *)
let repr_graph = visualgraph ();;

(* gscreen: content of the graphic screen *)
let gscreen = Array.make_matrix 64 128 false;;

(** Graphic display **)
let gdraw (ren : Sdlrender.t) : unit =
  parameters_updated := false;
  print_mat ren gscreen false
    (fun ren -> (* rect ren (!margin_h-1) (!margin_v-1) (!width+1) (!height+1) *) ());
  refresh ren;;

(** Graphic functions **)

(* Converts the coordinates x,y, in the system {xmin, xmax, ymin, ymax}, to a pixel in {1, 127, 1, 63} *)
(* First draft *)
let rescale (p : parameters) (x : float) (y : float) : int * int =
  let ax = (x -. p.xmin) /. (p.xmax -. p.xmin) in
  let ay = (y -. p.ymin) /. (p.ymax -. p.ymin) in
  (1 + true_int_of_float (ax *. 127.),
   1 + true_int_of_float (ay *. 63.));;

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
    (Array.iter (fun x -> let (i,j) = rescale p x 0. in ploton ren gscreen i (j+1)) step_xplus;
    Array.iter (fun x -> let (i,j) = rescale p x 0. in ploton ren gscreen i (j+1)) step_xminus);
  if not (is_zero_float p.ystep) then
    let nb_step_yplus  = true_int_of_float (p.ymax /. p.ystep) in
    let nb_step_yminus = true_int_of_float (p.ymin /. p.ystep) in
    let step_yplus = Array.init nb_step_yplus (fun i -> (float_of_int i)*.p.ystep) in
    let step_yminus = Array.init nb_step_yminus (fun i -> (float_of_int (-i))*.p.ystep) in
    (Array.iter (fun y -> let (i,j) = rescale p 0. y in ploton ren gscreen (i+1) j) step_yplus;
    Array.iter (fun y -> let (i,j) = rescale p 0. y in ploton ren gscreen (i+1) j) step_yminus);;

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
        ibeg := -1)
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
(* Each line is shifted by the given offset (0..64*128-1) *)
let draw_pict_offset (ren : Sdlrender.t) (p : parameters) (pict : int) (wanted_size : int) (offset : int) : unit =
  let (pict_size, m) = p.pict.(pict) in
  let actual_size = min 1024 (min pict_size wanted_size) in
  let nb_lines = actual_size / 16 in
  let rects = ref [] in
  let offset_i = offset mod 128 in
  let offset_j = offset / 64 in
  if offset = 0 then
    for j = 0 to nb_lines - 1 do
      aux_draw_line m j 0 127 rects
    done
  else
    for j = 0 to nb_lines - 1 do
      let actual_j = (j + offset_j) mod 64 in
      aux_draw_line m actual_j offset_i 127 rects;
      aux_draw_line m ((actual_j + 1) mod 64) 0 (offset_i-1) rects;
    done;
  let remainder = 8 * (actual_size mod 16) in
  if remainder <> 0 then
    let offset_rem = offset_i + remainder - 1 in
    let actual_nj = (nb_lines + offset_j) mod 64 in
    (aux_draw_line m actual_nj offset_i (min 127 offset_rem) rects;
    if offset_rem > 127 then
      aux_draw_line m ((actual_nj + 1) mod 64) 0 (offset_rem - 128) rects);
  Sdlrender.fill_rects ren (Array.of_list !rects);;

(* Auxiliary function that returns the next picture in reader order *)
let next_pict_index (i : int) : int =
  if i = 1 then 10
  else if i = 19 then 2
  else if i = 2 then 20
  else i + 1;;

(* General RlcPict function, handles the cascading effect of compressed pictures *)
let draw_pict (ren : Sdlrender.t) (p : parameters) (pict : int) : unit =
  let rec aux (i : int) (bytes_left : int) : unit =
    let (pict_size, m) = p.pict.(pict) in
    let wanted_size = min bytes_left pict_size in
    draw_pict_offset ren p pict wanted_size (2048 - bytes_left);
    if pict_size < bytes_left then
      aux (next_pict_index pict) (bytes_left - pict_size)
  in
  aux pict 2048;;

(* Draws the window in the current coordinates system *)
let draw_window (ren : Sdlrender.t) (p : parameters) : unit =
  wipe gscreen;
  if p.axeson then
    draw_axes ren p;
  if p.bgpict <> -1 then
    draw_pict_offset ren p (p.bgpict) 1024 0;;

(** Text display **)

(* Auxiliary loop to fast_locate:
  for k = bound downto i do (print the character at position k) done *)
let text_aux (init_i : int) (j : int) (acc : Sdlrect.t list ref) (l : string list) : int =
  let rec aux current_i l : int =
    match l with
      | s::lt ->
        let t =
          try
            Hashtbl.find repr_graph s
          with
            | Not_found -> Hashtbl.find repr_graph " "
        in
        (* Each character is 5 pixels tall and between 1 and 5 pixels wide *)
        let len = (Array.length t)/5 in
        (* If there is room for one more character, continue,
          else return the current abscissa *)
        if current_i + len <= 128 then
          (for y = 0 to 4 do
            for x = 0 to len-1 do
              if t.(len*y + x) then
                let px = current_i + x in
                let py = j + y in
                let r = fast_ploton px py in
                (gscreen.(py).(px) <- true;
                acc := r::!acc)
            done
          done;
          aux (current_i + len + 1) lt)
        else current_i
      | [] -> current_i
  in
  aux init_i l;;

(* Text display function *)
(* Prints the string s (stored as a list of lexemes in reverse order) in the gscreen at position i,j *)
let draw_text (ren : Sdlrender.t) (slist : string list) (i : int) (j : int) : unit =
  (* Computation of the pixels to be printed *)
  (* acc contains the rectangles passed to the renderer to be filled *)
  let acc = ref [] in
  let iend = text_aux i j acc (List.rev slist) in

  (* White rectangle to cover the area we write in *)
  set_color ren white;
  let white_r = Sdlrect.make2
    ~pos:(!margin_h + !size*i - 1, !margin_v + !size*j)
    ~dims:(!size*(iend - i + 1), 6 * !size)
  in
  Sdlrender.fill_rect ren white_r;
  set_color ren black;

  (* Display of the characters *)
  Sdlrender.fill_rects ren (Array.of_list !acc);;


(* Draws the number z (of type complex) on the graphic screen *)
(* polar = true if the complex is to be printed in polar form, false in a+ib form *)
let draw_number (ren : Sdlrender.t) (z : complex) (polar : bool) (i : int) (j : int) : unit =
  if z.im = 0. then
    let z_repr = float_to_casio z.re in
    draw_text ren (str_to_rev_symblist_simple z_repr) i j
  else
    let z_repr_l =
      if polar
        then complex_to_casio_polar z
        else if z.re = 0.
          then [[]; []; str_to_rev_symblist_simple (float_to_casio z.im); ["\127\080"]]
          else complex_to_casio_aib z
    in
    (* Concatenation of all four parts of z_repr_l into the whole string in reverse order *)
    let sl = List.fold_left (fun s l -> List.rev_append l s) [] z_repr_l in
    draw_text ren sl i j;;


  
        