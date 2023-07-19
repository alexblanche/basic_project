(* Floyd-Steinberg dithering algorithm *)

#use "bmp_reader.ml"
#use "picture_editor.ml"

(* Returns true if the color (r,g,b) is closer to black (pixel)
   and false if it is closer to white (no pixel) *)
(* Closer to black: (r+g+b)/3 <= 255*threshold
   threshold = 0.5 by default*)
let closest_color (r : int) (g : int) (b : int) (threshold : float) : bool =
  r+g+b <= int_of_float(765.*.threshold);;

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
    min (height/target_height) (width/target_width)
  in
  let rt = ref 0 in
  let gt = ref 0 in
  let bt = ref 0 in
  (* Computation of a three-color identical image of size 64*128 *)
  for j = 63 downto 0 do
    for i = 0 to 127 do
      rt := 0;
      gt := 0;
      bt := 0;
      for x = size*i to size*(i+1)-1 do
        for y = size*j to size*(j+1)-1 do
          let (r,g,b) = reverse_rgb (img.(y).(x)) in
          rt := !rt + r;
          gt := !gt + g;
          bt := !bt + b;
        done
      done;
      let rm = !rt / (size*size) in
      let gm = !gt / (size*size) in
      let bm = !bt / (size*size) in
      reduced_img.(j).(i) <- rgb rm gm bm
    done
  done;
  reduced_img;;

(* Auxiliary function to the Floyd-Steinberg algorithm *)
(* Recolors the (r,g,b) pixel (i,j) as (r+err_r*coef/16,...,...) *)
let error_propagation (img : image_mat) (i : int) (j : int) (err_r : int) (err_g : int) (err_b : int) (coef : int) : unit =
  let (r,g,b) = reverse_rgb (img.(j).(i)) in
  img.(j).(i) <- rgb (r+err_r*coef/16) (g+err_g*coef/16) (b+err_b*coef/16);;

(* Returns a 64*128 matrix of monochromatic pixels (of type bool array array)
  that approximates the image img through the Floyd-Steinberg algorithm *)
(* We assume that width/height >= 2 *)
let floyd_steinberg (img : image_mat) : bool array array =
  let rimg = reduce_img img 64 128 in (* Resized image (with RGB colors) *)
  let mono = Array.make_matrix 64 128 false in
  (* Treatment of the image, except the bottom line *)
  for j = 63 downto 1 do
    (* Leftmost line *)
    let (r,g,b) = reverse_rgb (rimg.(j).(0)) in
    let cc = closest_color r g b 0.5 in
    mono.(j).(0) <- cc;
    let c = (if cc then 0 else 255) in
    let err_r = r - c in
    let err_g = g - c in
    let err_b = b - c in
    error_propagation rimg 1 j err_r err_g err_b 7;
    error_propagation rimg 0 (j-1) err_r err_g err_b 5;
    error_propagation rimg 1 (j-1) err_r err_g err_b 1;
    for i = 1 to 126 do
      (* Interior of the image *)
      let (r,g,b) = reverse_rgb (rimg.(j).(i)) in
      let cc = closest_color r g b 0.5 in
      mono.(j).(i) <- cc;
      let c = (if cc then 0 else 255) in
      let err_r = r - c in
      let err_g = g - c in
      let err_b = b - c in
      error_propagation rimg (i+1) j err_r err_g err_b 7;
      error_propagation rimg (i-1) (j-1) err_r err_g err_b 3;
      error_propagation rimg i (j-1) err_r err_g err_b 5;
      error_propagation rimg (i+1) (j-1) err_r err_g err_b 1
    done;
    (* Rightmost line *)
    let (r,g,b) = reverse_rgb (rimg.(j).(127)) in
    let cc = closest_color r g b 0.5 in
    mono.(j).(127) <- cc;
    let c = (if cc then 0 else 255) in
    let err_r = r - c in
    let err_g = g - c in
    let err_b = b - c in
    error_propagation rimg 126 (j-1) err_r err_g err_b 3;
    error_propagation rimg 127 (j-1) err_r err_g err_b 5
  done;
  (* Treatment of the bottom line *)
  for i = 0 to 126 do
    let (r,g,b) = reverse_rgb (rimg.(0).(i)) in
    let cc = closest_color r g b 0.5 in
    mono.(0).(i) <- cc;
    let c = (if cc then 0 else 255) in
    let err_r = r - c in
    let err_g = g - c in
    let err_b = b - c in
    error_propagation rimg (i+1) 0 err_r err_g err_b 7
  done;
  (* Bottom-right pixel *)
  let (r,g,b) = reverse_rgb (rimg.(0).(127)) in
  mono.(0).(127) <- closest_color r g b 0.5;
  mono;;

(* Testing *)
let test_reduce (img : image_mat) =
  view_image img;
  let rimg = reduce_img img 64 128 in
  view_image rimg;;

(* Displays the original image and its monochromatic conversion side by side *)
let view_side_by_side (img : image_mat) (mono : bool array array) =
  let height = Array.length img
  and width = Array.length img.(0) in
  let margin = 20 in
  let size = min (height/64) (width/128) in
  let img_mono = mono_to_image_mat mono size in
  
  open_graph "";
  resize_window (3*margin + width + size*128) (2*margin + height);
  set_window_title "Monochromatic Conversion";
  auto_synchronize false;

  let pict_img = make_image img in
  let pict_mono = make_image img_mono in
  draw_image pict_img margin margin;
  set_color black;
  rect (2*margin + width) margin (128*size + 1) (64*size + 1);
  draw_image pict_mono (2*margin + width + 1) (margin + 1);
  synchronize ();
	
  let _ = read_key() in
  close_graph();;

(* Testing function *)
let test_fs (s : string) =
  (* let img = read_bmp "/mnt/c/users/blanc/desktop/ocaml_tests/basic_project/mandelbrot_bmp.bmp" in *)
  let img = read_bmp ("/mnt/c/users/blanc/desktop/ocaml_tests/basic_project/"^s^".bmp") in
  let mono = floyd_steinberg img in
  view_side_by_side img mono;;


(* Todo:
   - Debug (penguin.bmp)
   - do a better reduce (with black bars) *)