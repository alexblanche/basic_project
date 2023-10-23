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

(* bgscreen: background of the graphic screen (axes and BGPict) *)
let bgscreen = Array.make_matrix 64 128 false;;
(* gscreen: content of the graphic screen (everything but axes and BGPict) *)
let gscreen = Array.make_matrix 64 128 false;;

(* background_changed: used to indicate that a complete redraw of the screen should be done,
  because the background parameters were modified *)
let background_changed = ref true;;

(** Graphic functions **)

(* Converts the coordinates x,y, in the system {xmin, xmax, ymin, ymax}, to a pixel in {1, 127, 1, 63} *)
let rescale_x (p : parameters) (x : float) : int =
  let ax = 126. *. (x -. p.xmin) /. (p.xmax -. p.xmin) -. 0.5 in
  if ax >= 0.
    then true_int_of_float ax + 2
    else true_int_of_float ax + 1;;

let rescale_y (p : parameters) (y : float) : int =
  let ay = 62. *. (y -. p.ymin) /. (p.ymax -. p.ymin) -. 0.5 in
  if ay >= 0.
    then true_int_of_float ay + 2
    else true_int_of_float ay + 1;;

let rescale (p : parameters) (x : float) (y : float) : int * int =
  (rescale_x p x, rescale_y p y);;

(* Draws a horizontal line in the current system of coordinates at ordinate y *)
let draw_horizontal_line (ren : Sdlrender.t) (p : parameters) (y : float) : unit =
  let j = rescale_y p y in
  horizontal_line ren gscreen 1 127 j;;

(* Draws a vertical line in the current system of coordinates at abscissa x *)
let draw_horizontal_line (ren : Sdlrender.t) (p : parameters) (x : float) : unit =
  let i = rescale_x p x in
  vertical_line ren gscreen 1 63 i;;

(* Combines horizontal and vertical line drawings, but more efficient *)
(* Draws the scales (one plot per xstep, ystep on each axis) *)
let draw_axes (ren : Sdlrender.t) (p : parameters) : unit =
  let (i, j) = rescale p 0. 0. in
  let i_axis = i - 1 in
  let j_axis = 64 - j in
  (if j_axis >= 1 && j_axis <= 63 then
    horizontal_line ren bgscreen 1 127 j_axis);
  (if i_axis >= 1 && i_axis <= 127 then
    vertical_line ren bgscreen i_axis 1 63);
  (* Scales *)
  (* Horizontal scales *)
  if not (is_zero_float p.xstep) then
    (let nb_step_xplus  = true_int_of_float (Float.abs (p.xmax /. p.xstep)) in
    let nb_step_xminus = true_int_of_float (Float.abs (p.xmin /. p.xstep)) in
    let step_xplus = Array.init nb_step_xplus (fun i -> (float_of_int (i+1))*.p.xstep) in
    let step_xminus = Array.init nb_step_xminus (fun i -> (float_of_int (-i-1))*.p.xstep) in
    let j = rescale_y p 0. in
    let correctj =
      let temp_j = min (max 1 (63-j)) 63 in
      if temp_j = j_axis
        then (if temp_j = 1 then 2 else temp_j - 1)
        else temp_j
    in
    (Array.iter
      (fun x ->
        let i = rescale_x p x in
        if i >= 1 && i <= 127
          then ploton ren bgscreen i correctj)
      step_xplus;
    Array.iter
      (fun x ->
        let i = rescale_x p x in
        if i >= 1 && i <= 127
          then ploton ren bgscreen i correctj)
      step_xminus));

  (* Vertical scales *)
  if not (is_zero_float p.ystep) then
    (let nb_step_yplus  = true_int_of_float (Float.abs (p.ymax /. p.ystep)) in
    let nb_step_yminus = true_int_of_float (Float.abs (p.ymin /. p.ystep)) in
    let step_yplus = Array.init nb_step_yplus (fun j -> (float_of_int (j+1))*.p.ystep) in
    let step_yminus = Array.init nb_step_yminus (fun j -> (float_of_int (-j-1))*.p.ystep) in
    let i = rescale_x p 0. in
    let correcti =
      let temp_i = min (max 1 i) 127 in
      if temp_i = i_axis
        then (if temp_i = 1 then 2 else temp_i - 1)
        else temp_i
    in
    (Array.iter
      (fun y ->
        let j = rescale_y p y in
        if j >= 1 && j <= 63
          then ploton ren bgscreen correcti (64-j))
      step_yplus;
    Array.iter
      (fun y ->
        let j = rescale_y p y in
        if j >= 1 && j <= 63
          then ploton ren bgscreen correcti (64-j))
      step_yminus));;

(* Auxiliary function to draw_pict *)
(* Adds to rects the rectangles needed to draw line j of the picture stored in matrix m,
  between the abscissas imin and imax included *)
(* If write is true then each pixel is written in the matrix screen *)
let aux_draw_line (write : bool) (screen : bool array array) (m : bool array array) (j : int) (imin : int) (imax : int) (rects : Sdlrect.t list ref) : unit =
  let ibeg = ref (-1) in
  for i = imin to imax do
    if m.(j).(i) then
      (if !ibeg = (-1) then
        ibeg := i)
    else
      if !ibeg <> -1 then
        (rects := (horizontal_rect !ibeg (i-1) j)::!rects;
        if write then
          write_horizontal screen !ibeg (i-1) j;
        ibeg := -1)
  done;
  if !ibeg <> -1 then
    (rects := (horizontal_rect !ibeg imax j)::!rects;
    if write then
      write_horizontal screen !ibeg imax j);;

(* Draws the picture pict (0..19) on screen *)
(* The first given number of bytes is drawn:
  8 pixels per bytes, 16 bytes per line,
  1024 = normal picture, < 16 = less than one line *)
(* Each line is shifted by the given offset (0..64*128-1) *)
(* If write is true, for each pixel drawn, a "true" is placed in the corresponding cell
   of the screen matrix (gscreen or bgscreen) *)
let draw_pict_offset (ren : Sdlrender.t) (write : bool) (screen : bool array array)
  (pict_size : int) (wanted_size : int) (offset : int) (m : bool array array) : unit =

  let actual_size = min 1024 (min pict_size wanted_size) in
  let nb_lines = actual_size / 16 in
  let rects = ref [] in
  let offset_i = offset mod 128 in
  let offset_j = offset / 64 in
  (if offset = 0 then
    for j = 0 to nb_lines - 1 do
      aux_draw_line write screen m j 0 127 rects
    done
  else
    for j = 0 to nb_lines - 1 do
      let actual_j = (j + offset_j) mod 64 in
      aux_draw_line write screen m actual_j offset_i 127 rects;
      aux_draw_line write screen m ((actual_j + 1) mod 64) 0 (offset_i-1) rects;
    done);
  let remainder = 8 * (actual_size mod 16) in
  if remainder <> 0 then
    (let offset_rem = offset_i + remainder - 1 in
    let actual_nj = (nb_lines + offset_j) mod 64 in
    (aux_draw_line write screen m actual_nj offset_i (min 127 offset_rem) rects;
    if offset_rem > 127 then
      aux_draw_line write screen m ((actual_nj + 1) mod 64) 0 (offset_rem - 128) rects));
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
    if wanted_size <> 0 then
      draw_pict_offset ren true gscreen pict_size wanted_size (2048 - bytes_left) m;
    if pict_size < bytes_left && pict < 20 then
      aux (next_pict_index pict) (bytes_left - pict_size)
  in
  aux pict 2048;;

(* Draws a single picture mpict, without the cascading effect *)
(* If write is true, it writes the content of the picture in the matrix screen *)
(* The first 1024 bytes of the picture are drawn. *)
let draw_single_pict (ren : Sdlrender.t) (write : bool) (screen : bool array array)
  (mpict : bool array array) : unit =

  draw_pict_offset ren write screen 1024 1024 0 mpict;;

(* Same without writing *)
let draw_single_pict_no_writing (ren : Sdlrender.t) (mpict : bool array array) : unit =
  draw_single_pict ren false [||] mpict;;

(* Erases the black screen in graphic mode *)
let erase_black_square_graphic (ren : Sdlrender.t) : unit =
  set_color ren colors.background;
  draw_black_square ren;
  for j = 0 to 3 do
    for i = 124 to 127 do
      if bgscreen.(j).(i) || gscreen.(j).(i) then
        ploton_no_writing ren i j
    done
  done;
  set_color ren colors.pixels;;

(* Draws the window in the current coordinates system *)
let draw_window (ren : Sdlrender.t) (p : parameters) : unit =
  wipe bgscreen;
  if p.axeson then
    draw_axes ren p;
  if p.bgpict <> -1 then
    let (bgpict_size, m) = p.pict.(p.bgpict) in
    draw_pict_offset ren true bgscreen bgpict_size 1024 0 m;;

(* If the background parameters were modified, redraws the window,
  then refreshed the renderer *)
let refresh_update (ren : Sdlrender.t) (p : parameters) (text_screen : bool) : unit =
  (if !background_changed || text_screen then
    (* (print_endline "Updating..."; *)
    (clear_graph ren;
    draw_frame ren;
    draw_black_square ren;
    draw_window ren p;
    draw_single_pict_no_writing ren gscreen;
    background_changed := false));
  (* print_endline "Refreshing..."; *)
  refresh ren;;

(** Graphic display **)
let gdraw (ren : Sdlrender.t) : unit =
  parameters_updated := false;
  clear_graph ren;
  draw_frame ren;
  draw_black_square ren;
  draw_single_pict_no_writing ren bgscreen;
  draw_single_pict_no_writing ren gscreen;
  refresh ren;;

(** Text display **)

(* Auxiliary loop to fast_locate:
  for k = bound downto i do (print the character at position k) done *)
let text_aux (init_i : int) (j : int) (acc : Sdlrect.t list ref) (l : string list) : int * (int * int) list =
  let rec aux (current_i : int) (points : (int * int) list) (l : string list) : int * (int * int) list =
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
        if current_i + len <= 127 then
          (let new_points = ref [] in
          for y = 0 to 4 do
            for x = 0 to len-1 do
              if t.(len*y + x) then
                let px = current_i + x in
                let py = j + y in
                (new_points := (px,py) :: !new_points;
                acc := (ploton_rect px py)::!acc)
            done
          done;
          aux (current_i + len + 1) (List.rev_append !new_points points) lt)
        else (current_i, points)
      | [] -> (current_i, points)
  in
  aux init_i [] l;;

(* Text display function *)
(* Prints the string s (stored as a list of lexemes in reverse order) in the gscreen at position i,j *)
let draw_text (ren : Sdlrender.t) (slist : string list) (i : int) (j : int) : unit =
  (* Computation of the pixels to be printed *)
  (* acc contains the rectangles passed to the renderer to be filled *)
  let acc = ref [] in
  let (iend, points) = text_aux i j acc (List.rev slist) in

  (* White rectangle to cover the area we write in *)
  set_color ren colors.background;
  let white_r = Sdlrect.make2
    ~pos:(!margin_h + !size*i, !margin_v + !size*j)
    ~dims:(!size*(iend - i), 6 * !size)
  in
  Sdlrender.fill_rect ren white_r;

  (* Erasing the background of the text in the matrices *)
  for a = i to iend-1 do
    for b = j to j+5 do
      bgscreen.(b).(a) <- false;
      gscreen.(b).(a) <- false
    done
  done;

  (* Display of the characters *)
  List.iter (fun (x,y) -> gscreen.(y).(x) <- true) points;
  set_color ren colors.pixels;
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