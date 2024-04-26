(* Creation of 64*128 monochromatic images
  through the Floyd-Steinberg dithering algorithm
  and the Threshold algorithm applied on existing BMP images *)

(** Useful functions **)

(* Returns true if the color (r,g,b) is closer to black (pixel)
   and false if it is closer to white (no pixel) *)
let closest_color (r : int) (g : int) (b : int) : bool =
  let rf = float_of_int r in
  let gf = float_of_int g in
  let bf = float_of_int b in
  0.3*.rf +. 0.59*.gf +. 0.11*.bf <= 127.5;;

(* Same with a threshold *)
let closest_color_threshold (r : int) (g : int) (b : int) (threshold : float) : bool =
  let rf = float_of_int r in
  let gf = float_of_int g in
  let bf = float_of_int b in
  0.3*.rf +. 0.59*.gf +. 0.11*.bf <= 255. *. threshold;;


(** Conversion functions **)

(* Returns an image_mat in which each pixel of the monochromatic matrix mono
  is replaced by a square of side size *)
let mono_to_image_mat (mono : bool array array) (target_size : int) : image_mat =
  let mono_height = Array.length mono in
  let mono_width = Array.length mono.(0) in
  let img_height = target_size*mono_height in
  let img_width = target_size*mono_width in  
  let img = Array.make_matrix img_height img_width black in
  for im = 0 to mono_width-1 do
    for jm = 0 to mono_height-1 do
      let cm = if mono.(jm).(im) then black else white in
      for i = target_size*im to target_size*(im+1)-1 do
        for j = target_size*jm to target_size*(jm+1)-1 do
          img.(j).(i) <- cm
        done
      done
    done
  done;
  img;;

(* Returns an image of given size with averaged colors *)
(* target_height, target_width are supposed to be 64, 128 respectively for calculator screen *)
let reduce_img (img : image_mat) (target_height : int) (target_width : int) : image_mat =
  let height_img = Array.length img in
  let width_img = Array.length img.(0) in
  let reduced_img = Array.make_matrix target_height target_width black in
  let size_reduced =
    min (float_of_int(height_img)/.float_of_int(target_height)) (float_of_int(width_img)/.float_of_int(target_width))
  in
  let rt = ref 0 in
  let gt = ref 0 in
  let bt = ref 0 in
  (* The bounds of the blocks are computed with floats, to circumvent the rounding *)
  let xb = ref 0. in
  let yb = ref 0. in
  let cnt = ref 0 in (* Counter of the size of each block *)

  (* Computation of a RGB identical image of size 64*128 *)
  for j = 0 to target_height-1 do
    for i = 0 to target_width-1 do
      rt := 0;
      gt := 0;
      bt := 0;
      cnt := 0;
      for x = int_of_float !xb to int_of_float (!xb +. size_reduced)-1 do
        for y = int_of_float !yb to int_of_float (!yb +. size_reduced)-1 do
          let (r,g,b) = img.(y).(x) in
          rt := !rt + r;
          gt := !gt + g;
          bt := !bt + b;
          incr cnt
        done
      done;
      (* The number of cells in each block is approx. size*size, but it depends on roundings,
        hence the use of a counter *)
      let rm = !rt / !cnt in 
      let gm = !gt / !cnt in
      let bm = !bt / !cnt in
      reduced_img.(j).(i) <- (rm, gm, bm);
      xb := !xb +. size_reduced
    done;
    xb := 0.;
    yb := !yb +. size_reduced
  done;
  reduced_img;;


(** Floyd-Steinberg algorithm **)

(* Auxiliary function to the Floyd-Steinberg algorithm *)
(* Recolors the (r,g,b) pixel (i,j) as (r+err_r*coef/16,...,...) *)
let error_propagation (img : image_mat) (i : int) (j : int) (err_r : int) (err_g : int) (err_b : int) (coef : int) : unit =
  let (r,g,b) = img.(j).(i) in
  img.(j).(i) <- (r+err_r*coef/16, g+err_g*coef/16, b+err_b*coef/16);;

(* Returns a 64*128 matrix of monochromatic pixels (of type bool array array)
  that approximates the image rimg through the Floyd-Steinberg algorithm *)
(* We assume that rimg is a reduced image of size 64*128 *)
let floyd_steinberg (rimg : image_mat) : bool array array =
  (* Creation of a copy of rimg, that we will modify in place, so that the original rimg is untouched *)
  let img = Array.make_matrix 64 128 black in
  for i = 0 to 127 do
    for j = 0 to 63 do
      img.(j).(i) <- rimg.(j).(i)
    done
  done;
  let mono = Array.make_matrix 64 128 false in
  (* Treatment of the image, except the bottom line *)
  for j = 63 downto 1 do
    (* Leftmost line *)
    let (r,g,b) = img.(j).(0) in
    let cc = closest_color r g b in
    mono.(j).(0) <- cc;
    let c = (if cc then 0 else 255) in
    let err_r = r - c in
    let err_g = g - c in
    let err_b = b - c in
    error_propagation img 1 j err_r err_g err_b 7;
    error_propagation img 0 (j-1) err_r err_g err_b 5;
    error_propagation img 1 (j-1) err_r err_g err_b 1;
    for i = 1 to 126 do
      (* Interior of the image *)
      let (r,g,b) = img.(j).(i) in
      let cc = closest_color r g b in
      mono.(j).(i) <- cc;
      let c = (if cc then 0 else 255) in
      let err_r = r - c in
      let err_g = g - c in
      let err_b = b - c in
      error_propagation img (i+1) j err_r err_g err_b 7;
      error_propagation img (i-1) (j-1) err_r err_g err_b 3;
      error_propagation img i (j-1) err_r err_g err_b 5;
      error_propagation img (i+1) (j-1) err_r err_g err_b 1
    done;
    (* Rightmost line *)
    let (r,g,b) = img.(j).(127) in
    let cc = closest_color r g b in
    mono.(j).(127) <- cc;
    let c = (if cc then 0 else 255) in
    let err_r = r - c in
    let err_g = g - c in
    let err_b = b - c in
    error_propagation img 126 (j-1) err_r err_g err_b 3;
    error_propagation img 127 (j-1) err_r err_g err_b 5
  done;
  (* Treatment of the bottom line *)
  for i = 0 to 126 do
    let (r,g,b) = img.(0).(i) in
    let cc = closest_color r g b in
    mono.(0).(i) <- cc;
    let c = (if cc then 0 else 255) in
    let err_r = r - c in
    let err_g = g - c in
    let err_b = b - c in
    error_propagation img (i+1) 0 err_r err_g err_b 7
  done;
  (* Bottom-right pixel *)
  let (r,g,b) = img.(0).(127) in
  mono.(0).(127) <- closest_color r g b;
  mono;;


(** Threshold algorithm **)

(* Returns a 64*128 matrix of monochromatic pixels (of type bool array array)
  that approximates the image rimg through the threshold algorithm:
  each pixel (r,g,b) is set to white if (r+g+b)/3 < 255*threshold *)
(* We assume that rimg is a reduced image of size 64*128 *)
let img_to_mono_threshold (rimg : image_mat) (thresh : float) : bool array array =
  let mono = Array.make_matrix 64 128 false in
  for i = 0 to 127 do
    for j = 0 to 63 do
      let (r,g,b) = rimg.(j).(i) in
      mono.(j).(i) <- closest_color_threshold r g b thresh
    done
  done;
  mono;;


(** Interface **)

(* Displays the original image and its monochromatic conversion side by side *)
(* Returns either the Floyd-Steinberg or the Threshold image *)
let view_side_by_side (img : image_mat) : bool array array =
  let rimg = reduce_img img 64 128 in
  let mono_fs = floyd_steinberg rimg in
  let height_img = Array.length img in
  let width_img = Array.length img.(0) in
  let margin = 35 in
  let size = max (min (height_img/64) (width_img/128)) 3 in
  let img_mono_fs = mono_to_image_mat mono_fs size in
  (* let exit = ref false in *)
  (* let fs_mode = ref true in  *)
  (* let thresh = ref 0.5 in *)
  let mono_th = ref (img_to_mono_threshold rimg 0.5) in

  (* Window resizing counter *)
	let cpt_resize = ref 0 in
  
  let win =
		Sdlwindow.create
			~title:"Monochromatic Conversion"
			~pos:(`pos 10, `pos 10)
			~dims:(3 * margin + width_img + size * 128, 2 * margin + max height_img (size*64))
			~flags:[]
	in
	let ren =
		Sdlrender.create_renderer ~win ~index:0 ~flags:[]
	in
	Sdlrender.set_draw_blend_mode ren SdlblendMode.BNone;

  let bg () =
    draw_string ren 0 (margin + height_img + 5)
      "[a] to toggle Floyd-Steinberg/Threshold mode, [Esc] to quit." (0,0,0);
    draw_string ren 0 (margin + height_img + 20)
      "When in Threshold mode, [+]/[-] to increase/decrease the threshold." (0,0,0)
  in
  (* Obsolete *)
  (* let pict_img = make_image img in
  let pict_mono_fs = make_image img_mono_fs in *)

  let (img_txt, img_dstr) = make_texture ren img in
  let (monofs_txt, monofs_dstr) = make_texture ren img_mono_fs in

  (* Displays the image and the monochromatic version side by side *)
  (* Called by the main loop *)
  let draw fs_mode =
    (* Display of the colorful image and the monochromatic version (FS or threshold) *)
    clear_graph ren;
    bg ();
    draw_texture ren img_txt img_dstr margin margin;
    (* Red frame around the converted part of the original image *)
    set_color ren (255,0,0);
    (if width_img < height_img*2
      then (* Ratio < 2:1 (portrait, 16/9...) *)
        rect ren margin margin (width_img+1) ((width_img/2)+1)
      else (* Ratio >= 2:1 (flat landscape) *)
        rect ren margin margin ((2 * height_img)+1) (height_img+1));
    (* Black frame around the monochromatic image *)
    set_color ren black;
    rect ren (2*margin+width_img) (margin-1) (128*size+2) (64*size+2);
    (* Display of the monochromatic image *)
    if fs_mode
      then draw_texture ren monofs_txt monofs_dstr (2*margin + width_img + 1) margin
      else
        (* Obsolete *)
        (* let pict_mono_th = make_image (mono_to_image_mat !mono_th size) in
        draw_image pict_mono_th (2 * margin + width_img + 1) (margin + 1)); *)
        print_mat_content ren !mono_th (2 * margin + width_img + 1) (margin + 1) size;
    refresh ren
  in
  
  (* Input loop *)
  (* true if Floyd-Steinberg, false if Threshold mode *)
  let rec input_loop fs_mode thresh =
    match Sdlevent.poll_event () with
      (* Quitting *)
      | Some (Window_Event {kind = WindowEvent_Close})
      | Some (KeyDown {keycode = Escape}) ->
        (close_graph win;
        (* Output *)
        if fs_mode
          then mono_fs
          else !mono_th)

      (* Resizing *)
      (* To redo: make the whole view_side_by_side program depend on the global programs *)
			| Some (Window_Event {kind = WindowEvent_Resized wxy}) ->
        (incr cpt_resize;
        if !cpt_resize >= resize_threshold then
					(update_parameters wxy.win_x wxy.win_y;
					draw fs_mode;
					cpt_resize := 0);
        input_loop fs_mode thresh)

      | Some (Window_Event {kind = WindowEvent_Exposed})
      | Some Keymap_Changed ->
        (cpt_resize := resize_threshold;
				draw fs_mode; (* <- Useful ? *)
        input_loop fs_mode thresh)

			(* Input *)

      | Some (KeyDown {keycode = k}) ->
        let _ = wait_keyup k in
        if k = A then
          (flush_events ();
          draw (not fs_mode);
          input_loop (not fs_mode) thresh)
        else if not fs_mode then
          if k = KP_Plus && thresh < 1. then
            (flush_events ();
            let n_thresh = min 1. (thresh +. 0.05) in
            mono_th := img_to_mono_threshold rimg n_thresh;
            draw fs_mode;
            input_loop fs_mode n_thresh)
          else if k = KP_Minus && thresh > 0. then
            (flush_events ();
            let n_thresh = max 0. (thresh -. 0.05) in
            mono_th := img_to_mono_threshold rimg n_thresh;
            draw fs_mode;
            input_loop fs_mode n_thresh)
          else input_loop fs_mode thresh
        else input_loop fs_mode thresh

      | _ -> input_loop fs_mode thresh
  in

  let _ = draw true in
  input_loop true 0.5;;

(* Full interface: reads the BMP file, generates the monochromatic image
  through Floyd-Steinberg or the Threshold algorithm and edit the resulting image *)
(* Convention of the matrix output: (0,0) = lower-left corner. *)
let create_edit (file_name : string) : bool array array =
  let img = read_bmp file_name in
  let mono = view_side_by_side img in
  let _ = edit true mono in
  mono;;

(* Testing function *)
let test_fs (s : string) =
  (* let img = read_bmp "/mnt/c/users/blanc/desktop/ocaml_tests/basic_project/mandelbrot_bmp.bmp" in *)
  let img = read_bmp ("/mnt/c/users/blanc/desktop/ocaml_tests/basic_project/"^s^".bmp") in
  view_side_by_side img;;