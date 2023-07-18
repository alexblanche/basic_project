(* Floyd-Steinberg dithering algorithm *)

#use "bmp_reader.ml"
#use "picture_editor.ml"

(* Returns true if the color (r,g,b) is closer to black (pixel)
   and false if it is closer to white (no pixel) *)
let closest_color (r : int) (g : int) (b : int) : bool =
  2*(r+g+b) <= 3*255;;

(* Returns an image of size 64*128 with averaged colors *)
let reduce_img (img : image_mat) : image_mat =
  let height = Array.length img in
  let width = Array.length img.(0) in
  let reduced_img = Array.make_matrix 64 128 (0,0,0) in
  let size =
    min (height/64) (width/128)
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
          let (r,g,b) = img.(y).(x) in
          rt := !rt + r;
          gt := !gt + g;
          bt := !bt + b;
        done
      done;
      let rm = !rt / (size*size) in
      let gm = !gt / (size*size) in
      let bm = !bt / (size*size) in
      reduced_img.(j).(i) <- (rm,gm,bm)
    done
  done;
  reduced_img;;

(* Auxiliary function to the Floyd-Steinberg algorithm *)
(* Recolors the (r,g,b) pixel (i,j) as (r+err_r*coef/16,...,...) *)
let error_propagation (img : image_mat) (i : int) (j : int) (err_r : int) (err_g : int) (err_b : int) (coef : int) : unit =
  let (r,g,b) = img.(j).(i) in
  img.(j).(i) <- (r+err_r*coef/16, g+err_g*coef/16, b+err_b*coef/16);;

(* Returns a 64*128 matrix of monochromatic pixels (of type bool array array)
  that approximates the image img through the Floyd-Steinberg algorithm *)
(* We assume that width/height >= 2 *)
let floyd_steinberg (img : image_mat) : bool array array =
  let rimg = reduce_img img in (* Resized image (with RGB colors) *)
  let mono = Array.make_matrix 64 128 false in
  (* Treatment of the image, except the leftmost and rightmost lines,
    and the bottom line *)
  for j = 63 downto 1 do
    for i = 1 to 126 do
      let (r,g,b) = rimg.(j).(i) in
      let cc = closest_color r g b in
      (* print_endline ("("^string_of_int(i)^","^string_of_int(j)^") <- "^(if cc then "true" else "false")); *)
      mono.(j).(i) <- cc;
      let c = (if cc then 0 else 255) in
      let err_r = r - c in
      let err_g = g - c in
      let err_b = b - c in
      error_propagation rimg (i+1) j err_r err_g err_b 7;
      error_propagation rimg (i-1) (j-1) err_r err_g err_b 3;
      error_propagation rimg i (j-1) err_r err_g err_b 5;
      error_propagation rimg (i+1) (j-1) err_r err_g err_b 1
    done
  done;
  (* Treatment of the leftmost and rightmost lines *)
  for j = 63 downto 1 do
    let (r,g,b) = rimg.(j).(0) in
    mono.(j).(0) <- closest_color r g b;
    let (r,g,b) = rimg.(j).(127) in
    mono.(j).(127) <- closest_color r g b
  done;
  (* Treatment of the bottom line *)
  for i = 0 to 127 do
    let (r,g,b) = rimg.(0).(i) in
    mono.(0).(i) <- closest_color r g b
  done;
  mono;;

let test_reduce (img : image_mat) =
  view_image img;
  let rimg = reduce_img img in
  view_image rimg;;

(* TO BE MOVED IN picture_editor.ml AFTER TESTING *)
(* Displays the original image and its monochromatic conversion side by side *)
let view_side_by_side (img : image_mat) (mono : bool array array) =
  let height = Array.length img
  and width = Array.length img.(0) in
  let margin = 20 in
  let halfmargin = margin/2 in
  
  open_graph "";
  resize_window (2*margin + 2*width) (margin + height);
  set_window_title "Monochromatic Conversion";
  auto_synchronize false;

  for j = 0 to height-1 do
    for i = 0 to width-1 do
      let (r, g, b) = img.(j).(i) in
      set_color (rgb r g b);
      plot (i + halfmargin) (j + halfmargin)
    done
  done; 

  set_color black;
  let size = max 1 (min (height/64) (width/128)) in
  rect (margin + halfmargin + width) halfmargin (128*size) (64*size);
	
  let ibeg = ref 0 in
	let i = ref 0 in
	for j = 0 to 63 do
		i := 0;
		ibeg := 0;
		while !i<>128 && (not mono.(j).(!i)) do
			incr i
		done;
		while !i < 128 do
			while !i<>128 && (not mono.(j).(!i)) do
				incr i
			done;
			ibeg := !i;
			while !i<>128 && mono.(j).(!i) do
				incr i
			done;
			fill_rect (margin + halfmargin + width + size * !ibeg) (halfmargin + size*j) ((!i - !ibeg)*size) size;
			incr i
		done;
	done;
  synchronize();
  let _ = read_key() in
  close_graph();;

let test_fs () =
  (* let img = read_bmp "/mnt/c/users/blanc/desktop/ocaml_tests/basic_project/mandelbrot_bmp.bmp" in *)
  let img = read_bmp "/mnt/c/users/blanc/desktop/ocaml_tests/basic_project/data/peng2.bmp" in
  let mono = floyd_steinberg img in
  view_side_by_side img mono;;
(* Testing function *)
(* Works fine on Mondelbrot_bmp.bmp, peng2.bmp *)
(* Does not work on penguin.bmp (why?) *)

(* Todo:
   - Debug (penguin.bmp)
   - rework left, right, bottom lines
   - do a better reduce (with black bars) *)