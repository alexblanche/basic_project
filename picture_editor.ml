(* Creation and manipulation of 128*64 monochrome pictures  *)

#use "topfind"
#require "graphics"
open Graphics;;

(* Note: in the Graphics library, (0,0) is in the lower-left corner of the graphics window *)

(* Constants *)
let margin = 40;; (* Margin between the graphics window and the screen *)
let size = 7;; (* Size of a pixel of the screen *)
let width = 128*size;;
let height = 64*size;;

let marginf = float_of_int margin;;
let sizef = float_of_int size;;
let widthf = float_of_int width;;
let heightf = float_of_int height;;

(* Geometric vector *)
(* (x,y)
	x belongs to [0.,widthf[
	y belongs to [0.,heightf[ *)
type vector = float * float;;

(* Cell of the screen:
	 (i,j)
	 i belongs to 0..127
	 j belongs to 0..63 *)
type cell = int * int;;

(* Returns the vector ab = b - a *)
let vect (a : vector) (b : vector) : vector =
	let (xa,ya) = a
	and (xb,yb) = b in
	(xb-.xa, yb-.ya);;

(* Returns the vector at the center of the cell (i,j) *)
let cell_to_vector (c : cell) : vector =
	let (i,j) = c in
	(float_of_int (margin+size*i)+.(sizef/.2.), float_of_int (margin+size*j)+.(sizef/.2.));;

(* Returns the cell that contains the vector (x,y) *)
let vector_to_cell (v : vector) : cell =
	let (x,y) = v in
	if x<marginf || x>marginf+.widthf || y<marginf || y>marginf+.heightf
		then (0,0)
		else ((int_of_float (x-.marginf))/size, (int_of_float (y-.marginf))/size)

(* Returns the vector v scaled by factor scal *)
let ( // ) (v : vector) (scal : float) =
	let (x,y) = v in
	(scal*.x, scal*.y);;

(* Returns the norm of vector v *)
let norm (v : vector) : float =
	let (x,y) = v in
	sqrt ((x*.x)+.(y*.y));;

(* Returns the vector v applied to point pt *)
let extr (pt : vector) (v : vector) : vector =
	let (a,b) = pt
	and (x,y) = v in
	(a+.x, b+.y);;

(* Returns a unit vector colinear with v *)
let unit_vect v =
	let n = norm v in
	v // (1./.n);;

(* Setup functions *)
let cache () =
	display_mode false;
	remember_mode true;;

let sync () = synchronize();;

(* Traces a rectangle of width w and height h, with lower-left point (a,b) *)
let rect (a : int) (b : int) (w : int) (h : int) =
	let (c,d) = (a+w, b+h) in
	moveto a b;
	lineto c b;
	lineto c d;
	lineto a d;
	lineto a b;;

(* Traces a line from (a,b) to (x,y) on the graphics window *)
let dline (a : int) (b : int) (x : int) (y : int) =
	moveto a b;
	lineto x y;;

(* Fills the pixel x,y of the screen with the current color *)
let plot (i : int) (j : int) =
	fill_rect (margin+size*i) (margin+size*j) size size;;

(* Auxiliary function to the function f_line *)
(* m is the pixel matrix,
	i,j is the current position
	k,l is the destination position *)
let rec f_line_aux m i j k l =
	
	(*print_string "f_line : x,y = ";
	print_int x; print_string ","; print_int y;
	print_string " - a,b = ";
	print_int a; print_string ","; print_int b;
	print_newline();*)
	
	let ptij = cell_to_vector (i,j) in
	let ptkl = cell_to_vector (k,l) in
	let v = vect ptij ptkl in
	let v' = (unit_vect v) // 16. in
	let vbis = (unit_vect v) // 20. in

	let x,y = ptij in
	let pt = extr ptij v' in
	let ptbis = extr ptij vbis in
	
	let (a,b) = ptbis in
	let (c,d) = vector_to_cell pt in
	
	m.(d).(c) <- true;
	plot c d;
		
	set_color red;
	dline (int_of_float x) (int_of_float y) (int_of_float a) (int_of_float b);
	set_color black;
		
	if c<>k || d<>l then
		f_line_aux m c d k l;;

(* Traces a line between pixel i,j and k,l
	m is the pixel matrix *)
let f_line m i j k l =
	cache();
	plot i j;
	m.(j).(i) <- true;
	f_line_aux m i j k l;
	sync();;


let f_linebis m x y a b =
	cache();
	let ptxy = cell_to_vector (x,y) in
	let v = vect ptxy (cell_to_vector (a,b)) in
	let k = int_of_float ((norm v)/.7.) in
	for i=0 to k do
	let p = float_of_int i in
		let (a,b) = extr ptxy (v//(p/.(float_of_int k))) in
		let (c,d) = vector_to_cell (a,b) in
		m.(d).(c) <- true;
		plot c d;
	done;
	sync();;

(** Graphical interface **)

let grey = rgb 120 120 148;;

(* Opens and configure the graphical window *)
let config (grid : bool) : unit =
	open_graph "";
	resize_window (2*margin + width) (2*margin + height);
	cache ();
	
	(* frame *)
	set_color black;
	rect margin margin width height;
	sync();
	
	(* grid *)
	if grid
		then
			(set_color grey;
			for i=1 to 127 do
				dline (margin+size*i) margin (margin+size*i) (margin+height)
			done;
			for j=1 to 63 do
				dline margin (margin+size*j) (margin+width) (margin+size*j)
			done;
			sync();
			set_color black);;



(* Returns a 128*64 matrix of pixels *)
let interface (grid : bool) : bool array array =
	
	config grid;
	let m = Array.make_matrix 64 128 false in	
	let exit = ref false in
	let a_pressed = ref None in
	
	while not !exit do
	
		let {mouse_x; mouse_y; button; keypressed; key} =
			wait_next_event [Button_down; Key_pressed]
		in
		exit := key = '\027';
		
		if button then
		begin
			let {mouse_x; mouse_y; button; keypressed; key} =
				wait_next_event [Button_up; Poll]
			in

			(match !a_pressed with
				| None -> ()
				| Some (i,j) ->
					(a_pressed := None;
					set_color white;
					plot i j;
					sync();
					set_color black));
			
			(* display of the point on which we clicked *)
			let (i,j) = vector_to_cell (float_of_int mouse_x, float_of_int mouse_y) in
			plot i j;
			sync();
			m.(j).(i) <- true;
		end;
		
		if keypressed && key = 'a' then
			match !a_pressed with
				| None ->
					begin
						let (i,j) = vector_to_cell (float_of_int mouse_x, float_of_int mouse_y) in
						set_color red;
						plot i j;
						sync();
						set_color black;
						a_pressed := Some (i,j)
					end
				| Some (i,j) ->
					begin
						let k,l = vector_to_cell (float_of_int mouse_x, float_of_int mouse_y) in
						if k<>i || j<>l
							then
								(f_linebis m i j k l;
								a_pressed := None)
					end
	done;
	close_graph ();
	m;;

let print_mat (m : bool array array) : unit =
	config false;
	set_color black;
	cache();
	for i=0 to 63 do
		for j=0 to 127 do
			if m.(i).(j) then
				plot j i;
		done;
	done;
	sync();
	let _ = wait_next_event [Button_down; Key_pressed] in
	close_graph ();;

(* Todo:
	remove all useless casts to float
	take care of plot off (white pixels leave a hole in the grid)
	take care of out of bounds

	implement
	- Bresenham algorithm (to trace a line)
	- Floyd-Steinberg algorithm (to convert to actual monochrome) *)

