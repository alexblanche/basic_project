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

let gray = rgb 120 120 148;;

(* Cell of the screen:
	 (i,j)
	 i belongs to 0..127
	 j belongs to 0..63 *)
type cell = int * int;;

exception Out_of_screen

(* Returns the cell that contains the vector (x,y) *)
let coord_to_cell (x : int) (y : int) : cell =
	if x<margin || x>margin+width || y<margin || y>margin+height
		then raise Out_of_screen
		else ((x-margin)/size, ((y-margin)/size));;

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
let plot (m : bool array array) (i : int) (j : int) =
	fill_rect (margin+size*i) (margin+size*j) size size;
	m.(j).(i) <- true;;

(* Fills the pixel x,y of the screen with white and reforms the grid *)
let plotoff (m : bool array array) (grid : bool) (i : int) (j : int) =
	if m.(j).(i) then
		(set_color white;
		fill_rect (margin+size*i) (margin+size*j) size size;
		m.(j).(i) <- false;
		if grid then
			(* reform the grid around the deleted pixel *)
			(set_color gray;
			rect (margin+size*i) (margin+size*j) size size;
			set_color black));;


(* Traces an horizontal line between pixel (i1,j) and (i2,j) (i1 is the leftmost pixel) *)
let horitzontal_line (m : bool array array) (i1 : int) (i2 : int) (j : int) =
	fill_rect (margin+size*i1) (margin+size*j) ((i2-i1+1)*size) size;
	for i = i1 to i2 do
		m.(j).(i) <- true
	done;;

(* Traces a vertical line between pixel (i,j1) and (i,j2) (j1 is the lowest pixel) *)
let vertical_line (m : bool array array) (i : int) (j1 : int) (j2 : int) =
	fill_rect (margin+size*i) (margin+size*j1) size ((j2-j1+1)*size);
	for j = j1 to j2 do
		m.(j).(i) <- true
	done;;


(* Bresenham algorithm *)

(* 1st and 8th octants *)
let bresenham_loop_18 (m : bool array array) (i1 : int) (j1 : int) (i2 : int) (j2 : int) =
	let di = i2 - i1 in
	let e = ref di in (* e>0 *)
	let ddi = 2*di in
	let ddj = 2*(j2 - j1) in
	(* sdj = 1 : 1st octant
		sdj = -1 : 8th octant *)
	let sdj = if ddj > 0 then 1 else -1 in
	let j = ref j1 in
	(* for i = i1 to i2-1 do
		plot i !j;
		e := !e - sdj*ddj;
		if !e < 0 then
			(j := !j + sdj;
			e:= !e + ddi);
	done; *)
	let i = ref i1 in
	(* We trace the horizontal segments in one call *)
	let ibeg = ref i1 in
	while !i < i2 do
		ibeg := !i;
		e := !e - sdj*ddj;
		while !e>=0 && !i<>i2-1 do
			e := !e - sdj*ddj;
			incr i
		done;
		horitzontal_line m !ibeg !i !j;
		j := !j + sdj;
		e:= !e + ddi;
		incr i
	done;
	plot m i2 j2;;

(* 4th and 5th octants *)
let bresenham_loop_45 (m : bool array array) (i1 : int) (j1 : int) (i2 : int) (j2 : int) =
	let di = i2 - i1 in
	let e = ref di in (* e<0 *)
	let ddi = 2*di in
	let ddj = 2*(j2 - j1) in
	(* sdj = 1 : 4th octant
		sdj = -1 : 5th octant *)
	let sdj = if ddj > 0 then 1 else -1 in
	let j = ref j1 in
	(* for i = i1 downto i2+1 do
		plot i !j;
		e := !e + sdj*ddj;
		if !e >= 0 then
			(j := !j + sdj;
			e:= !e + ddi);
	done; *)
	let i = ref i1 in
	let ibeg = ref i1 in
	while !i > i2 do
		ibeg := !i;
		e := !e + sdj*ddj;
		while !e<0 && !i<>i2+1 do
			e := !e + sdj*ddj;
			decr i
		done;
		horitzontal_line m !i !ibeg !j;
		j := !j + sdj;
		e:= !e + ddi;
		decr i
	done;
	plot m i2 j2;;

(* 2nd and 3rd octants *)
let bresenham_loop_23 (m : bool array array) (i1 : int) (j1 : int) (i2 : int) (j2 : int) =
	let dj = j2 - j1 in
	let e = ref dj in (* e>0 *)
	let ddi = 2*(i2-i1) in
	let ddj = 2*dj in
	(* sdi = 1 : 2nd octant
		sdi = -1 : 3rd octant *)
	let sdi = if ddi > 0 then 1 else -1 in
	let i = ref i1 in
	let j = ref j1 in
	let jbeg = ref j1 in
	while !j < j2 do
		jbeg := !j;
		e := !e - sdi*ddi;
		while !e>=0 && !j<>j2-1 do
			e := !e - sdi*ddi;
			incr j
		done;
		vertical_line m !i !jbeg !j;
		i := !i + sdi;
		e:= !e + ddj;
		incr j
	done;
	plot m i2 j2;;

(* 6th and 7th octants *)
let bresenham_loop_67 (m : bool array array) (i1 : int) (j1 : int) (i2 : int) (j2 : int) =
	let dj = j2 - j1 in
	let e = ref dj in (* e<0 *)
	let ddi = 2*(i2-i1) in
	let ddj = 2*dj in
	(* sdi = -1 : 6th octant
		sdi = 1 : 7th octant *)
	let sdi = if ddi > 0 then 1 else -1 in
	let i = ref i1 in
	let j = ref j1 in
	let jbeg = ref j1 in
	while !j > j2 do
		jbeg := !j;
		e := !e + sdi*ddi;
		while !e<0 && !j<>j2+1 do
			e := !e + sdi*ddi;
			decr j
		done;
		vertical_line m !i !j !jbeg;
		i := !i + sdi;
		e:= !e + ddj;
		decr j
	done;
	plot m i2 j2;;

let bresenham (m : bool array array) (i1 : int) (j1 : int) (i2 : int) (j2 : int) =
	let di = i2 - i1 in
	let dj = j2 - j1 in
	(match (di=0),(di>0),(dj=0),(dj>0),(di>=dj),(di>=(-dj)),(-di>dj),(di<=dj) with
		| true,_,true,_,_,_,_,_ -> plot m i1 j1 (* single point *)
		| true,_,_,true,_,_,_,_ -> vertical_line m i1 j1 j2 (* vertical ascending *)
		| true,_,_,_,_,_,_,_ -> vertical_line m i1 j2 j1 (* vertical descending *)
		| _,true,true,_,_,_,_,_ -> horitzontal_line m i1 i2 j1 (* horizontal right *)
		| _,true,_,true,true,_,_,_ -> bresenham_loop_18 m i1 j1 i2 j2 (* 1st octant *)
		| _,true,_,true,_,_,_,_ -> bresenham_loop_23 m i1 j1 i2 j2 (* 2nd octant *)
		| _,true,_,_,_,true,_,_ -> bresenham_loop_18 m i1 j1 i2 j2 (* 8th octant *)
		| _,true,_,_,_,_,_,_ -> bresenham_loop_67 m i1 j1 i2 j2 (* 7th octant *)
		| _,_,true,_,_,_,_,_ -> horitzontal_line m i2 i1 j1 (* horitzontal left *)
		| _,_,_,true,_,_,true,_ -> bresenham_loop_45 m i1 j1 i2 j2 (* 4th octant *)
		| _,_,_,true,_,_,_,_ -> bresenham_loop_23 m i1 j1 i2 j2 (* 3rd octant *)
		| _,_,_,_,_,_,_,true -> bresenham_loop_45 m i1 j1 i2 j2 (* 5th octant *)
		| _ -> bresenham_loop_67 m i1 j1 i2 j2 (* 6th octant *));;


(** Graphical interface **)

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
			(set_color gray;
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
					plotoff m grid i j;
					sync ()));
			
			(* display of the point on which we clicked *)
			try
				let (i,j) = coord_to_cell mouse_x mouse_y in
				plot m i j;
				sync()
			with
				| Out_of_screen -> ();
			
		end;
		
		if keypressed then
			if key = 'a' then
				match !a_pressed with
					| None ->
						(try
							let (i,j) = coord_to_cell mouse_x mouse_y in
							set_color red;
							plot m i j;
							sync ();
							set_color black;
							a_pressed := Some (i,j)
						with
							| Out_of_screen -> ())
					| Some (i,j) ->
							try
								let (k,l) = coord_to_cell mouse_x mouse_y in
								if k<>i || j<>l	then
									(bresenham m i j k l;
									sync();
									a_pressed := None)
							with
								| Out_of_screen -> ()
			else if key = 'd' then
				try
					let (i,j) = coord_to_cell mouse_x mouse_y in
					plotoff m grid i j;
					sync ()
				with
					| Out_of_screen -> ()
	done;
	close_graph ();
	m;;

let print_mat (m : bool array array) : unit =
	config false;
	set_color black;
	cache();
	for j=0 to 63 do
		for i=0 to 127 do
			if m.(j).(i) then
				plot m i j;
		done;
	done;
	sync();
	let _ = wait_next_event [Button_down; Key_pressed] in
	close_graph ();;

(* implement
	- Floyd-Steinberg algorithm (to convert to actual monochrome) *)

