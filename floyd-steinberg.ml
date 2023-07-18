(* Floyd-Steinberg dithering algorithm *)

#use "bmp_reader.ml"
#use "picture_editor.ml"

(* Returns true if the color (r,g,b) is closer to white
   and false if it is closer to black *)
let closest_color (r : int) (g : int) (b : int) : bool =
  2*(r+g+b) >= 3*256;;

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

(* Returns a 64*128 matrix of monochromatic pixels (of type bool array array)
  that approximates the image img through the Floyd-Steinberg algorithm *)
(* We assume that width/height >= 2 *)
(* let floyd_steinberg (img : image) : bool array array =
  let rimg = reduce_img img in
  for j = 63 to 1 do
    for i = 0 to 126 do
;; *)

(* let test_reduce (img : image_mat) =
  view_image img;
  let rimg = reduce_img img in
  view_image rimg;; *)

(* TO BE MOVED IN picture_editor.ml AFTER TESTING *)
(* Displays the original image and its monochromatic conversion side by side *)
let view_side_by_side (img : image_mat) (mono : bool array array) =
  let height = Array.length img
  and width = Array.length img.(0) in
  let margin = 40 in
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
  let size = height/64 in
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
			while !i<>128 && (not mono.(j).(!i))  do
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
(* Untested *)
