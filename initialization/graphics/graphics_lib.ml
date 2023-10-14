(* Creation and manipulation of 64*128 monochrome pictures *)
(* Uses SDL2 and SDL2_TTF libraries *)


(* Closes the window win *)
let close_graph (win : Sdlwindow.t) : unit =
	Sdlwindow.destroy win;;

(* Refresh function *)
let refresh (ren : Sdlrender.t) : unit =
	Sdlrender.render_present ren;;

(* Sets drawing color *)
let set_color (ren : Sdlrender.t) (color : int * int * int) : unit =
	Sdlrender.set_draw_color ren ~rgb:color ~a:255;;

(* Clears the window *)
let clear_graph (ren : Sdlrender.t) : unit =
	set_color ren white;
  Sdlrender.clear ren;
	set_color ren black;;

(* Draws the string s at position x,y with color rgb in the given renderer *)
let draw_string (ren : Sdlrender.t) (x : int) (y : int) (s: string) (rgb : int * int * int) : unit =
	let (r,g,b) = rgb in
	let surface = Sdlttf.render_text_solid font ~text:s ~color:{Sdlttf.r = r; g = g; b = b; a = 255} in
	let textw, texth = Sdlsurface.get_dims surface in
	let texture =	Sdltexture.create_from_surface ren surface in
	Sdlsurface.free surface;
	let dst_rect = Sdlrect.make ~pos:(x, y) ~dims:(textw, texth) in
	Sdlrender.copy ren ~texture ~dst_rect ();
	Sdltexture.destroy texture;;

(* Inefficient version, going through a surface *)
(* Copies the screen into a texture *)
(* Returns the texture and the destination rectangle *)
(* let make_texture (win : Sdlwindow.t) (ren : Sdlrender.t) : Sdltexture.t * Sdlrect.t =
	let surf = Sdlwindow.get_surface win in
	let () = Sdlrender.read_pixels ren surf in
	let texture = Sdltexture.create_from_surface ren surf in
	let dims = Sdlsurface.get_dims surf in
	let rect = Sdlrect.make2 ~pos:(0,0) ~dims in
	(texture, rect);; *)

(* Creates a texture representing the image img *)
(* Returns the texture and the destination rectangle *)
let make_texture (ren : Sdlrender.t) (img : image_mat) : Sdltexture.t * Sdlrect.t =
	let height_img = Array.length img in
	let width_img = Array.length img.(0) in
	let texture =
		Sdltexture.create ren SdlpixelFormat.RGB888
			SdltextureAccess.Target width_img height_img
	in
	(* Setting the rendering target to the texture *)
	Sdlrender.set_render_target ren (Some texture);
	(* Rendering the image in the texture *) 
	for j = 0 to height_img - 1 do
		for i = 0 to width_img - 1 do
			Sdlrender.set_draw_color ren ~rgb:img.(j).(i) ~a:255;
			Sdlrender.draw_point ren (i,j)
		done
	done;
	(* Resetting the rendering target to the window *)
	Sdlrender.set_render_target ren None;
	let dst_rect = Sdlrect.make2 ~pos:(0,0) ~dims:(width_img, height_img) in
	(texture, dst_rect);;

(* Displays the texture in the renderer ren *)
let draw_texture (ren : Sdlrender.t) (texture : Sdltexture.t) (dst_rect : Sdlrect.t) (x : int) (y : int) : unit =
	Sdlrender.copy ren ~texture ~dst_rect:(Sdlrect.move dst_rect ~x ~y) ();;

(* Traces a rectangle of width w and height h, with lower-left point (a,b) *)
let rect (ren : Sdlrender.t) (a : int) (b : int) (w : int) (h : int) =
	Sdlrender.draw_rect ren (Sdlrect.make ~pos:(a,b) ~dims:(w,h));;

(* Traces a line from (a,b) to (x,y) on the graphics window *)
let dline (ren : Sdlrender.t) (a : int) (b : int) (x : int) (y : int) =
	Sdlrender.draw_line ren ((a,b),(x,y));;

(* Fills a rectangle of width w and height h, with lower-left point (a,b) *)
let fill_rect (ren : Sdlrender.t) (a : int) (b : int) (w : int) (h : int) =
	Sdlrender.fill_rect ren (Sdlrect.make ~pos:(a,b) ~dims:(w,h));;

(* Fills the pixel i,j of the screen with the current color *)
let ploton (ren : Sdlrender.t) (m : bool array array) (i : int) (j : int) =
	fill_rect ren (!margin_h + !size*i) (!margin_v + !size*j) !size !size;
	m.(j).(i) <- true;;

(* Same without writing in a matrix *)
let ploton_no_writing (ren : Sdlrender.t) (i : int) (j : int) =
	fill_rect ren (!margin_h + !size*i) (!margin_v + !size*j) !size !size;;

(* Fills the pixel i,j of the screen with white and reforms the grid *)
let plotoff (ren : Sdlrender.t) (m : bool array array) (grid : bool) (i : int) (j : int) =
	if m.(j).(i) then
		(set_color ren white;
		fill_rect ren (!margin_h + !size*i) (!margin_v + !size*j) !size !size;
		m.(j).(i) <- false;
		if grid then
			(* reform the grid around the deleted pixel *)
			(set_color ren gray;
			rect ren (!margin_h + !size*i) (!margin_v + !size*j) (!size+1) (!size+1));
		set_color ren black);;


(* Traces an horizontal line between pixel (i1,j) and (i2,j) (i1 is the leftmost pixel) *)
let horizontal_line (ren : Sdlrender.t) (m : bool array array) (i1 : int) (i2 : int) (j : int) =
	fill_rect ren (!margin_h + !size*i1) (!margin_v + !size*j) ((i2-i1+1) * !size) !size;
	for i = i1 to i2 do
		m.(j).(i) <- true
	done;;

(* Traces a vertical line between pixel (i,j1) and (i,j2) (j1 is the lowest pixel) *)
let vertical_line (ren : Sdlrender.t) (m : bool array array) (i : int) (j1 : int) (j2 : int) =
	fill_rect ren (!margin_h + !size*i) (!margin_v + !size*j1) !size ((j2-j1+1) * !size);
	for j = j1 to j2 do
		m.(j).(i) <- true
	done;;

(* Traces a rectangle with lower-left cell (i1,j1) and upper-right cell (i2,j2) *)
let rec rectangle_no_writing (ren : Sdlrender.t) (i1 : int) (j1 : int) (i2 : int) (j2 : int) =
	if i1 > i2 || j1 > j2
		then rectangle_no_writing ren (min i1 i2) (min j1 j2) (max i1 i2) (max j1 j2)
		else fill_rect ren (!margin_h + !size*i1) (!margin_v + !size*j1) ((i2-i1+1) * !size) ((j2-j1+1) * !size);;

(* Traces a rectangle with lower-left cell (i1,j1) and upper-right cell (i2,j2)
	and writes the pixel down in the matrix m *)
let rectangle (ren : Sdlrender.t) (m : bool array array) (i1 : int) (j1 : int) (i2 : int) (j2 : int) =
	rectangle_no_writing ren i1 j1 i2 j2;
	let i0 = min i1 i2 in
	let il = max i1 i2 in
	let j0 = min j1 j2 in
	let jl = max j1 j2 in 
	for i = i0 to il do
		for j = j0 to jl do
			m.(j).(i) <- true
		done
	done;;

(* Bresenham algorithm *)

(* 1st and 8th octants *)
let bresenham_loop_18 (ren : Sdlrender.t) (m : bool array array) (i1 : int) (j1 : int) (i2 : int) (j2 : int) =
	let di = i2 - i1 in
	let e = ref di in (* e>0 *)
	let ddi = 2*di in
	let ddj = 2*(j2 - j1) in
	(* sdj = 1 : 1st octant
		sdj = -1 : 8th octant *)
	let sdj = if ddj > 0 then 1 else -1 in
	let j = ref j1 in
	(* for i = i1 to i2-1 do
		ploton i !j;
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
		horizontal_line ren m !ibeg !i !j;
		j := !j + sdj;
		e:= !e + ddi;
		incr i
	done;
	ploton ren m i2 j2;;

(* 4th and 5th octants *)
let bresenham_loop_45 (ren : Sdlrender.t) (m : bool array array) (i1 : int) (j1 : int) (i2 : int) (j2 : int) =
	let di = i2 - i1 in
	let e = ref di in (* e<0 *)
	let ddi = 2*di in
	let ddj = 2*(j2 - j1) in
	(* sdj = 1 : 4th octant
		sdj = -1 : 5th octant *)
	let sdj = if ddj > 0 then 1 else -1 in
	let j = ref j1 in
	(* for i = i1 downto i2+1 do
		ploton i !j;
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
		horizontal_line ren m !i !ibeg !j;
		j := !j + sdj;
		e:= !e + ddi;
		decr i
	done;
	ploton ren m i2 j2;;

(* 2nd and 3rd octants *)
let bresenham_loop_23 (ren : Sdlrender.t) (m : bool array array) (i1 : int) (j1 : int) (i2 : int) (j2 : int) =
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
		vertical_line ren m !i !jbeg !j;
		i := !i + sdi;
		e:= !e + ddj;
		incr j
	done;
	ploton ren m i2 j2;;

(* 6th and 7th octants *)
let bresenham_loop_67 (ren : Sdlrender.t) (m : bool array array) (i1 : int) (j1 : int) (i2 : int) (j2 : int) =
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
		vertical_line ren m !i !j !jbeg;
		i := !i + sdi;
		e:= !e + ddj;
		decr j
	done;
	ploton ren m i2 j2;;

let bresenham (ren : Sdlrender.t) (m : bool array array) (i1 : int) (j1 : int) (i2 : int) (j2 : int) =
	let di = i2 - i1 in
	let dj = j2 - j1 in
	(match (di=0),(di>0),(dj=0),(dj>0),(di>=dj),(di>=(-dj)),(-di>dj),(di<=dj) with
		| true,_,true,_,_,_,_,_ 	-> ploton ren m i1 j1 (* single point *)
		| true,_,_,true,_,_,_,_ 	-> vertical_line ren m i1 j1 j2 (* vertical ascending *)
		| true,_,_,_,_,_,_,_ 			-> vertical_line ren m i1 j2 j1 (* vertical descending *)
		| _,true,true,_,_,_,_,_ 	-> horizontal_line ren m i1 i2 j1 (* horizontal right *)
		| _,true,_,true,true,_,_,_ -> bresenham_loop_18 ren m i1 j1 i2 j2 (* 1st octant *)
		| _,true,_,true,_,_,_,_ 	-> bresenham_loop_23 ren m i1 j1 i2 j2 (* 2nd octant *)
		| _,true,_,_,_,true,_,_ 	-> bresenham_loop_18 ren m i1 j1 i2 j2 (* 8th octant *)
		| _,true,_,_,_,_,_,_ 			-> bresenham_loop_67 ren m i1 j1 i2 j2 (* 7th octant *)
		| _,_,true,_,_,_,_,_ 			-> horizontal_line ren m i2 i1 j1 (* horizontal left *)
		| _,_,_,true,_,_,true,_ 	-> bresenham_loop_45 ren m i1 j1 i2 j2 (* 4th octant *)
		| _,_,_,true,_,_,_,_ 			-> bresenham_loop_23 ren m i1 j1 i2 j2 (* 3rd octant *)
		| _,_,_,_,_,_,_,true 			-> bresenham_loop_45 ren m i1 j1 i2 j2 (* 5th octant *)
		| _ 											-> bresenham_loop_67 ren m i1 j1 i2 j2 (* 6th octant *));;


(** Graphical interface **)

(* Prints the background and the grid *)
let print_bg (ren : Sdlrender.t) (grid : bool) (bg : Sdlrender.t -> unit) : unit =
	(* grid *)
	if grid
		then
			(set_color ren gray;
			let y0, yh = !margin_v, !margin_v + !height in
			for i=1 to 127 do
				let x = !margin_h + !size*i in
				dline ren x y0 x yh
			done;
			let x0, xw = !margin_h, !margin_h + !width in
			for j=1 to 63 do
				let y = !margin_v + !size*j in
				dline ren x0 y xw y
			done;
			set_color ren black);
	
	(* frame *)
	set_color ren black;
	rect ren (!margin_h-1) (!margin_v-1) (!width+1) (!height+1);
	
	(* Additional background *)
	bg ren;;

(* Opens and configure the graphical window *)
(* grid = true the display the grid in the background
	 bg: function that prints additional things in the background *)
(* Returns the window and the renderer *)
let config (grid : bool) (bg : Sdlrender.t -> unit) : Sdlwindow.t * Sdlrender.t =
	let window =
		Sdlwindow.create
			~title:"Basic Project Inferface"
			~pos:(`pos 400, `pos 200)
			~dims:(2 * !margin_h + !width, 2 * !margin_v + !height)
			~flags:[]
	in
	let renderer =
		Sdlrender.create_renderer ~win:window ~index:0 ~flags:[]
	in
	Sdlrender.set_draw_blend_mode renderer SdlblendMode.BNone;
	
	print_bg renderer grid bg;
	refresh renderer;
	(window, renderer);;


(* Displays the matrix m on the screen at position (x,y) with pixels of given size *)
(* Does not refresh the window *)
let print_mat_content (ren : Sdlrender.t) (m : bool array array) (x : int) (y : int) (size : int) : unit =
	let ibeg = ref 0 in
	let i = ref 0 in
	for j = 0 to 63 do
		i := 0;
		ibeg := 0;
		while !i<>128 && (not m.(j).(!i)) do
			incr i
		done;
		while !i < 128 do
			while !i<>128 && (not m.(j).(!i))  do
				incr i
			done;
			ibeg := !i;
			while !i<>128 && m.(j).(!i) do
				incr i
			done;
			fill_rect ren (x + size * !ibeg) (y + size * j) ((!i - !ibeg) * size) size;
			incr i
		done;
	done;;

(* Displays the matrix m on the screen with the grid (if grid = true) and the background bg *)
(* Does not open a window, nor refresh it *)
let print_mat (ren : Sdlrender.t) (m : bool array array) (grid : bool) (bg : Sdlrender.t -> unit) : unit =
	clear_graph ren;
	print_bg ren grid bg;
	print_mat_content ren m !margin_h !margin_v !size;;
	
(* Opens a new graphic window and displays the matrix m *)
let view_matrix (m : bool array array) : unit =
	let (win, ren) = config false (fun _ -> ()) in
	set_color ren black;
	print_mat ren m false (fun _ -> ());
	refresh ren;
	(* Wait for a key to be pressed, or the window to be closed *)
	let rec loop () =
		match Sdlevent.poll_event () with
			| Some (Window_Event {kind = WindowEvent_Close})
			| Some KeyDown _ -> ()
			| _ -> loop ()
	in
	loop ();
	close_graph win;
	Sdl.quit ();;