(* Creation and manipulation of 64*128 monochrome pictures *)
(* Uses SDL2 and SDL2_TTF libraries *)

(* Closes the window win *)
let close_graph (win : Sdlwindow.t) (ren : Sdlrender.t) : unit =
	(* Sdlmissing. *)destroy_renderer ren;
	Sdlwindow.destroy win;;

(* Refresh function *)
let refresh (ren : Sdlrender.t) : unit =
	Sdlrender.render_present ren;;

(* Sets drawing color *)
let set_color (ren : Sdlrender.t) (color : int * int * int) : unit =
	Sdlrender.set_draw_color ren ~rgb:color ~a:255;;

(* Clears the window *)
let clear_graph (ren : Sdlrender.t) : unit =
	set_color ren colors.background;
  Sdlrender.clear ren;
	set_color ren colors.pixels;;

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

(** PlotOn, PlotOff **)

(* Returns the rectangle plotted by ploton *)
let ploton_rect (i : int) (j : int) : Sdlrect.t =
	Sdlrect.make ~pos:(!margin_h + !size*i, !margin_v + !size*j) ~dims:(!size, !size);;

(* Fills the pixel i,j of the screen with the current color *)
let ploton (ren : Sdlrender.t) (m : bool array array) (i : int) (j : int) : unit =
	if i >= 0 && i <= 127 && j >= 0 && j <= 63 then
		(Sdlrender.fill_rect ren (ploton_rect i j);
		m.(j).(i) <- true);;
	(* else print_endline ("Runtime warning: PlotOn on out of screen parameters ("^(string_of_int i)^", "^(string_of_int j)^")");; *)

(* Same without writing in a matrix *)
let ploton_no_writing (ren : Sdlrender.t) (i : int) (j : int) : unit =
	Sdlrender.fill_rect ren (ploton_rect i j);;

(* Fills the pixel i,j of the screen with white and reforms the grid *)
let plotoff (ren : Sdlrender.t) (m : bool array array) (grid : bool) (i : int) (j : int) : unit =
	if m.(j).(i) then
		(set_color ren colors.background;
		Sdlrender.fill_rect ren (ploton_rect i j);
		m.(j).(i) <- false;
		if grid then
			(* reform the grid around the deleted pixel *)
			(set_color ren colors.grid;
			rect ren (!margin_h + !size*i) (!margin_v + !size*j) (!size+1) (!size+1));
		set_color ren colors.pixels);;

(** Horizontal, Vertical lines **)

(* Returns the rectangle plotted by Horizontal *)
let horizontal_rect (i1 : int) (i2 : int) (j : int) : Sdlrect.t =
	Sdlrect.make ~pos:(!margin_h + !size*i1, !margin_v + !size*j) ~dims:((i2-i1+1) * !size, !size);;

(* Returns the rectangle plotted by Vertical *)
let vertical_rect (i : int) (j1 : int) (j2 : int) : Sdlrect.t =
	Sdlrect.make ~pos:(!margin_h + !size*i, !margin_v + !size*j1) ~dims:(!size, (j2-j1+1) * !size);;

(* Writes in the matrix m the pixels of the horizontal line *)
let write_horizontal (m : bool array array) (i1 : int) (i2 : int) (j : int) : unit =
	for i = i1 to i2 do
		m.(j).(i) <- true
	done;;

(* Writes in the matrix m the pixels of the vertical line *)
let write_vertical (m : bool array array) (i : int) (j1 : int) (j2 : int) : unit =
	for j = j1 to j2 do
		m.(j).(i) <- true
	done;;

(* Traces an horizontal line between pixel (i1,j) and (i2,j) (i1 is the leftmost pixel) *)
let horizontal_line (ren : Sdlrender.t) (m : bool array array) (i1 : int) (i2 : int) (j : int) : unit =
	Sdlrender.fill_rect ren (horizontal_rect i1 i2 j);
	write_horizontal m i1 i2 j;;

(* Traces a vertical line between pixel (i,j1) and (i,j2) (j1 is the lowest pixel) *)
let vertical_line (ren : Sdlrender.t) (m : bool array array) (i : int) (j1 : int) (j2 : int) : unit =
	Sdlrender.fill_rect ren (vertical_rect i j1 j2);
	write_vertical m i j1 j2;;

(* Traces a rectangle with lower-left cell (i1,j1) and upper-right cell (i2,j2) *)
let rec rectangle_no_writing (ren : Sdlrender.t) (i1 : int) (j1 : int) (i2 : int) (j2 : int) : unit =
	if i1 > i2 || j1 > j2
		then rectangle_no_writing ren (min i1 i2) (min j1 j2) (max i1 i2) (max j1 j2)
		else fill_rect ren (!margin_h + !size*i1) (!margin_v + !size*j1) ((i2-i1+1) * !size) ((j2-j1+1) * !size);;

(* Traces a rectangle with lower-left cell (i1,j1) and upper-right cell (i2,j2)
	and writes the pixel down in the matrix m *)
let rectangle (ren : Sdlrender.t) (m : bool array array) (i1 : int) (j1 : int) (i2 : int) (j2 : int) : unit =
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

(* Reverse rectangle function: returns the coordinates of the pixels that generate
	a ploton rectangle or horizontal or vertical line *)
(* Returns (i,j,w,h), where (i,j) is the pixel (0..127, 0..63) at the top left of the rectangle,
	w and h are the width and height in number of pixels *)
let pixels_of_rectangle (rect : Sdlrect.t) : int * int * int * int =
	let i = (rect.x - !margin_h) / !size in
	let j = (rect.y - !margin_v) / !size in
	let w = rect.w / !size in
	let h = rect.h / !size in
	(i, j, w, h);;

(** Bresenham algorithm **)

(* Each function returns a list of rectangles, that should be plotted by the calling function *)
(* If write is true, each plot is written in the matrix m as true *)

(* 1st and 8th octants *)
(* For 1st/8th and 2nd/3rd octants: experimentally, in Basic Casio, these lines are reversed
	from the bresenham algorithm.
	i,j are replaced by (i1+i2-i),(j1+j2-j)
	This implementation seems pixel perfect (see tests/tests_run.ml). *)
let bresenham_loop_18 (write : bool) (m : bool array array)
	(i1 : int) (j1 : int) (i2 : int) (j2 : int) : Sdlrect.t list =

	let rect_l = ref [] in
	let di = i2 - i1 in
	let e = ref di in (* e>0 *)
	let ddi = 2*di in
	let ddj = 2*(j2 - j1) in
	let i1pi2 = i1 + i2 in
	let j1pj2 = j1 + j2 in
	(* sdj = 1 : 1st octant
		sdj = -1 : 8th octant *)
	let sdj = if ddj > 0 then 1 else -1 in
	let j = ref j1 in
	let ibeg = ref i1 in
	for i = i1 to i2 do
		e := !e - sdj*ddj;
		if !e < 0 || i = i2 then
			(rect_l := (horizontal_rect (i1pi2-i) (i1pi2 - !ibeg) (j1pj2 - !j))::!rect_l;
			(if write then
				write_horizontal m (i1pi2-i) (i1pi2 - !ibeg) (j1pj2 - !j));
			j := !j + sdj;
			e := !e + ddi;
			ibeg := i+1)
	done;
	!rect_l;;

(* 4th and 5th octants *)
let bresenham_loop_45 (write : bool) (m : bool array array)
	(i1 : int) (j1 : int) (i2 : int) (j2 : int) : Sdlrect.t list =

	let rect_l = ref [] in
	let di = i2 - i1 in
	let e = ref di in (* e<0 *)
	let ddi = 2*di in
	let ddj = 2*(j2 - j1) in
	(* sdj = 1 : 4th octant
		sdj = -1 : 5th octant *)
	let sdj = if ddj > 0 then 1 else -1 in
	let j = ref j1 in
	let ibeg = ref i1 in
	for i = i1 downto i2 do
		e := !e + sdj*ddj;
		if !e >= 0 || i = i2 then
			(rect_l := (horizontal_rect i !ibeg !j)::!rect_l;
			(if write then
				write_horizontal m i !ibeg !j);
			j := !j + sdj;
			e := !e + ddi;
			ibeg := i-1)
	done;
	!rect_l;;

(* 2nd and 3rd octants *)
(* Reversed, like bresenham_loop_18 *)
let bresenham_loop_23 (write : bool) (m : bool array array)
	(i1 : int) (j1 : int) (i2 : int) (j2 : int) : Sdlrect.t list =

	let rect_l = ref [] in
	let dj = j2 - j1 in
	let e = ref dj in (* e>0 *)
	let ddi = 2*(i2 - i1) in
	let ddj = 2*dj in
	let i1pi2 = i1 + i2 in
	let j1pj2 = j1 + j2 in
	(* sdi = 1 : 2nd octant
		sdi = -1 : 3rd octant *)
	let sdi = if ddi > 0 then 1 else -1 in
	let i = ref i1 in
	let jbeg = ref j1 in
	for j = j1 to j2 do
		e := !e - sdi*ddi;
		if !e < 0 || j = j2 then
			(rect_l := (vertical_rect (i1pi2 - !i) (j1pj2 - j) (j1pj2 - !jbeg))::!rect_l;
			(if write then
				write_vertical m (i1pi2 - !i) (j1pj2 - j) (j1pj2 - !jbeg));
			i := !i + sdi;
			e := !e + ddj;
			jbeg := j+1)
	done;
	!rect_l;;

(* 6th and 7th octants *)
let bresenham_loop_67 (write : bool) (m : bool array array)
	(i1 : int) (j1 : int) (i2 : int) (j2 : int) : Sdlrect.t list =

	let rect_l = ref [] in
	let dj = j2 - j1 in
	let e = ref dj in (* e<0 *)
	let ddi = 2*(i2 - i1) in
	let ddj = 2*dj in
	(* sdi = -1 : 6th octant
		sdi = 1 : 7th octant *)
	let sdi = if ddi > 0 then 1 else -1 in
	let i = ref i1 in
	let jbeg = ref j1 in
	for j = j1 downto j2 do
		e := !e + sdi*ddi;
		if !e >= 0 || j = j2 then
			(rect_l := (vertical_rect !i j !jbeg)::!rect_l;
			(if write then
				write_vertical m !i j !jbeg);
			i := !i + sdi;
			e := !e + ddj;
			jbeg := j-1)
	done;
	!rect_l;;

(* General Bresenham function *)
let bresenham (write : bool) (m : bool array array)
	(i1 : int) (j1 : int) (i2 : int) (j2 : int) : Sdlrect.t list =
	let di = i2 - i1 in
	let dj = j2 - j1 in
	(match (di=0),(di>0),(dj=0),(dj>0),(di>=dj),(di>=(-dj)),(-di>dj),(di<=dj) with
		| true,_,true,_,_,_,_,_ 	-> (* single point *)
			(if write
				then m.(j1).(i1) <- true;
			[ploton_rect i1 j1]) 
		| true,_,_,true,_,_,_,_ 	-> (* vertical ascending *)
			(if write
				then write_vertical m i1 j1 j2;
			[vertical_rect i1 j1 j2])
		| true,_,_,_,_,_,_,_ 			-> (* vertical descending *)
			(if write
				then write_vertical m i1 j2 j1;
			[vertical_rect i1 j2 j1])
		| _,true,true,_,_,_,_,_ 	-> (* horizontal right *)
			(if write
				then write_horizontal m i1 i2 j1;
			[horizontal_rect i1 i2 j1])
		| _,true,_,true,true,_,_,_ -> bresenham_loop_18 write m i1 j1 i2 j2 (* 1st octant *)
		| _,true,_,true,_,_,_,_ 	-> bresenham_loop_23 write m i1 j1 i2 j2 (* 2nd octant *)
		| _,true,_,_,_,true,_,_ 	-> bresenham_loop_18 write m i1 j1 i2 j2 (* 8th octant *)
		| _,true,_,_,_,_,_,_ 			-> bresenham_loop_67 write m i1 j1 i2 j2 (* 7th octant *)
		| _,_,true,_,_,_,_,_ 			-> (* horizontal left *)
			(if write
				then write_horizontal m i2 i1 j1;
			[horizontal_rect i2 i1 j1])
		| _,_,_,true,_,_,true,_ 	-> bresenham_loop_45 write m i1 j1 i2 j2 (* 4th octant *)
		| _,_,_,true,_,_,_,_ 			-> bresenham_loop_23 write m i1 j1 i2 j2 (* 3rd octant *)
		| _,_,_,_,_,_,_,true 			-> bresenham_loop_45 write m i1 j1 i2 j2 (* 5th octant *)
		| _ 											-> bresenham_loop_67 write m i1 j1 i2 j2 (* 6th octant *));;


(* Writing the pixels of rectangles in the gscreen matrix *)
let write_in_matrix (m : bool array array) (r : Sdlrect.t) : unit =
	let (i,j,w,h) = pixels_of_rectangle r in
  for a = i to i+w-1 do
    for b = j to j+h-1 do
  	  m.(b).(a) <- true
    done
  done;;
	



(** Graphical interface **)

(* Draws the frame around the screen *)
let draw_frame (ren : Sdlrender.t) : unit =
	rect ren (!margin_h-1) (!margin_v-1) (!width+2) (!height+2);;

(* Draws black square that appears at the top right of the screen when the program is running *)
let draw_black_square (ren : Sdlrender.t) : unit =
	Sdlrender.fill_rect ren (Sdlrect.make ~pos:(!margin_h + !size * 124, !margin_v) ~dims:(4 * !size, 4 * !size));;

(* Prints the background and the grid *)
let print_bg (ren : Sdlrender.t) (grid : bool) (bg : Sdlrender.t -> unit) : unit =
	(* grid *)
	if grid then
		(set_color ren colors.grid;
		let y0, yh = !margin_v, !margin_v + !height - 1 in
		for i=1 to 127 do
			let x = !margin_h + !size*i -1 in
			dline ren x y0 x yh
		done;
		let x0, xw = !margin_h, !margin_h + !width - 1 in
		for j=1 to 63 do
			let y = !margin_v + !size*j -1 in
			dline ren x0 y xw y
		done);
	
	(* frame *)
	set_color ren colors.pixels;
	draw_frame ren;
	
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
			~flags:[Sdlwindow.Resizable]
	in
	let renderer =
		Sdlrender.create_renderer ~win:window ~index:(-1) ~flags:[]
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
	set_color ren colors.pixels;
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
	close_graph win ren;
	Sdl.quit ();;