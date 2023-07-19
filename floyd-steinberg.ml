(* Floyd-Steinberg dithering algorithm *)

#use "bmp_reader.ml"
#use "picture_editor.ml"

(* Returns true if the color (r,g,b) is closer to black (pixel)
   and false if it is closer to white (no pixel) *)
let closest_color (r : int) (g : int) (b : int) : bool =
  let rf = float_of_int r in
  let gf = float_of_int g in
  let bf = float_of_int b in
  (0.3*.rf +. 0.59*.gf +. 0.11*.bf) <= 127.5;;

(* Same with a threshold *)
let closest_color_threshold (r : int) (g : int) (b : int) (threshold : float) : bool =
  let rf = float_of_int r in
  let gf = float_of_int g in
  let bf = float_of_int b in
  (0.3*.rf +. 0.59*.gf +. 0.11*.bf) <= 255. *. threshold;;

(* Returns the tuple (r,g,b) associated with the color c *)
let reverse_rgb (c : Graphics.color) : int * int * int =
  let b = c mod 256 in
  let c2 = c / 256 in
  (c2 / 256, c2 mod 256, b);;

(* Returns an image_mat in which each pixel of the monochromatic matrix mono
  is replaced by a square of side size *)
let mono_to_image_mat (mono : bool array array) (size : int) : image_mat =
  let mono_height = Array.length mono in
  let mono_width = Array.length mono.(0) in
  let img_height = size*mono_height in
  let img_width = size*mono_width in  
  let img = Array.make_matrix img_height img_width black in
  for im = 0 to mono_width-1 do
    for jm = 0 to mono_height-1 do
      let cm = if mono.(jm).(im) then black else white in
      for i = size*im to size*(im+1)-1 do
        for j = size*jm to size*(jm+1)-1 do
          img.(j).(i) <- cm
        done
      done
    done
  done;
  img;;

(* Returns an image of given size with averaged colors *)
(* target_height, target_width are supposed to be 64, 128 respectively for calculator screen *)
let reduce_img (img : image_mat) (target_height : int) (target_width : int) : image_mat =
  let height = Array.length img in
  let width = Array.length img.(0) in
  let reduced_img = Array.make_matrix target_height target_width black in
  let size =
    min (float_of_int(height)/.float_of_int(target_height)) (float_of_int(width)/.float_of_int(target_width))
  in
  let rt = ref 0 in
  let gt = ref 0 in
  let bt = ref 0 in
  (* The bounds of the blocks are computed with floats, to circumvent the rounding *)
  let xb = ref 0. in
  let yb = ref 0. in
  let cnt = ref 0 in (* Counter of the size of each block *)

  (* Computation of a three-color identical image of size 64*128 *)
  for j = 0 to target_height-1 do
    for i = 0 to target_width-1 do
      rt := 0;
      gt := 0;
      bt := 0;
      cnt := 0;
      for x = int_of_float !xb to int_of_float (!xb +. size)-1 do
        for y = int_of_float !yb to int_of_float (!yb +. size)-1 do
          let (r,g,b) = reverse_rgb (img.(y).(x)) in
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
      reduced_img.(j).(i) <- rgb rm gm bm;
      xb := !xb +. size
    done;
    xb := 0.;
    yb := !yb +. size
  done;
  reduced_img;;

(* Auxiliary function to the Floyd-Steinberg algorithm *)
(* Recolors the (r,g,b) pixel (i,j) as (r+err_r*coef/16,...,...) *)
let error_propagation (img : image_mat) (i : int) (j : int) (err_r : int) (err_g : int) (err_b : int) (coef : int) : unit =
  let (r,g,b) = reverse_rgb (img.(j).(i)) in
  img.(j).(i) <- rgb (r+err_r*coef/16) (g+err_g*coef/16) (b+err_b*coef/16);;

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
    let (r,g,b) = reverse_rgb (img.(j).(0)) in
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
      let (r,g,b) = reverse_rgb (img.(j).(i)) in
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
    let (r,g,b) = reverse_rgb (img.(j).(127)) in
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
    let (r,g,b) = reverse_rgb (img.(0).(i)) in
    let cc = closest_color r g b in
    mono.(0).(i) <- cc;
    let c = (if cc then 0 else 255) in
    let err_r = r - c in
    let err_g = g - c in
    let err_b = b - c in
    error_propagation img (i+1) 0 err_r err_g err_b 7
  done;
  (* Bottom-right pixel *)
  let (r,g,b) = reverse_rgb (img.(0).(127)) in
  mono.(0).(127) <- closest_color r g b;
  mono;;

(* Returns a 64*128 matrix of monochromatic pixels (of type bool array array)
  that approximates the image rimg through the threshold algorithm:
  each pixel (r,g,b) is set to white if (r+g+b)/3 < 255*threshold *)
(* We assume that rimg is a reduced image of size 64*128 *)
let img_to_mono_threshold (rimg : image_mat) (thresh : float) : bool array array =
  let mono = Array.make_matrix 64 128 false in
  for i = 0 to 127 do
    for j = 0 to 63 do
      let (r,g,b) = reverse_rgb (rimg.(j).(i)) in
      mono.(j).(i) <- closest_color_threshold r g b thresh
    done
  done;
  mono;;

(* Testing *)
let test_reduce (img : image_mat) =
  view_image img;
  let rimg = reduce_img img 64 128 in
  view_image rimg;;

(* Displays the original image and its monochromatic conversion side by side *)
let view_side_by_side (img : image_mat) =
  let rimg = reduce_img img 64 128 in
  let mono_fs = floyd_steinberg rimg in
  let height = Array.length img
  and width = Array.length img.(0) in
  let margin = 35 in
  let size = max (min (height/64) (width/128)) 5 in
  let img_mono_fs = mono_to_image_mat mono_fs size in
  let exit = ref false in
  let fs_mode = ref true in (* true if Floyd-Steinberg, false if Threshold mode *)
  let thresh = ref 0.5 in
  let mono_th = ref (img_to_mono_threshold rimg !thresh) in
  
  open_graph "";
  resize_window (3*margin + width + size*128) (2*margin + max height (size*64));
  set_window_title "Monochromatic Conversion";
  auto_synchronize false;
  let pict_img = make_image img in
  let pict_mono_fs = make_image img_mono_fs in
  let bg () =
    set_color black;
    moveto 0 15;
    draw_string "[a] to toggle Floyd-Steinberg/Threshold mode, [Esc] to quit.";
    moveto 0 0;
    draw_string "When in Threshold mode, [+]/[-] to increase/decrease the threshold."
  in

  while not !exit do
    clear_graph ();
    bg ();
    draw_image pict_img margin margin;
    (* Red frame around the converted part of the original image *)
    set_color red;
    (if width < height*2
      then (* Ratio < 2:1 (portrait, 16/9...) *)
        rect margin (margin + height - width/2) width (width/2)
      else (* Ratio >= 2:1 (flat landscape) *)
        rect margin margin (2*height) height);
    (* Black frame around the monochromatic image *)
    set_color black;
    rect (2*margin + width) margin (128*size + 1) (64*size + 1);
    (* Display of the monochromatic image *)
    (if !fs_mode
      then draw_image pict_mono_fs (2*margin + width + 1) (margin + 1)
      else
        let pict_mono_th = make_image (mono_to_image_mat !mono_th size) in
        draw_image pict_mono_th (2*margin + width + 1) (margin + 1));
    synchronize ();

    let {mouse_x; mouse_y; button; keypressed; key} =
			wait_next_event [Button_down; Key_pressed]
		in
		exit := key = '\027'; (* Esc *)
    if keypressed then
      if key = 'a' then
        fs_mode := not !fs_mode
      else if not !fs_mode then
        if key = '+' && !thresh <> 1. then
          (thresh := min 1. (!thresh +. 0.05);
          mono_th := img_to_mono_threshold rimg !thresh)
        else if key = '-' && !thresh <> 0. then
          (thresh := max 0. (!thresh -. 0.05);
          mono_th := img_to_mono_threshold rimg !thresh)
  done;
  close_graph();
  if !fs_mode
    then mono_fs
    else !mono_th;;

(* Testing function *)
let test_fs (s : string) =
  (* let img = read_bmp "/mnt/c/users/blanc/desktop/ocaml_tests/basic_project/mandelbrot_bmp.bmp" in *)
  let img = read_bmp ("/mnt/c/users/blanc/desktop/ocaml_tests/basic_project/"^s^".bmp") in
  view_side_by_side img;;

(* Todo:
   - Debug "Graphics.Graphic_failure "Xlib error: BadPixmap (invalid Pixmap parameter)"" on penguin.
   - Threshold looks a lot like FS......
  *)