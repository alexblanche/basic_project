(* Picture editing interface *)

(* Cell of the screen:
	 (i,j)
	 i belongs to 0..127
	 j belongs to 0..63 *)
type cell = int * int;;

exception Out_of_screen

(* Returns the cell that contains the vector (x,y) *)
let coord_to_cell (x : int) (y : int) : cell =
	if x <= !margin_h || x >= !margin_h + !width || y <= !margin_v || y >= !margin_v + !height
		then raise Out_of_screen
		else ((x - !margin_h) / !size, ((y - !margin_v) / !size));;

(* Interface that lets the user add pixels, lines, rectangles
	 and delete pixels on a 64*128 monochormatic matrix *)
(* Modifies the matrix in place *)
let edit (grid : bool) (m : bool array array) : unit =
	
	let instr ren =
		draw_string ren 0 (!margin_v + !height + 5)
			"[Left-mouse] to add pixels, hold to draw a line/rectangle, [Right-mouse] to delete pixels"
			(0,0,0);
		draw_string ren 0 (!margin_v + !height + 20)
		"[a] to toggle line/rectangle, [Esc] to quit"
			(0,0,0)
	in
	
	let (win, ren) = config grid instr in

	(* Unused matrix in which bresenham writes *)
	let mblank = Array.make_matrix 64 128 false in
	
	(* line = true if we draw a line, false if we draw a rectangle *)
	let line = ref true in
	let current_pixel = ref (-1,-1) in

	(* Window resizing counter *)
	let cpt_resize = ref 0 in

	let draw () =
		print_mat ren m grid instr;
		refresh ren
	in
	draw ();

	(* Loop that displays the previsualized line/rectangle until the mouse button is released *)
	let loop_previs (mbb : int) : unit =
		let (i0,j0) = !current_pixel in
		let click = ref true in

		while !click do
			(* Required for Sdlmouse.get_state to return a non-zero value *)
			let _ = flush_events () in
			let ((x,y), bl) = Sdlmouse.get_state () in
			click :=
				(if mbb = 1
					then (List.mem Sdlmouse.Button_Left bl)
					else if mbb = 3 then (List.mem Sdlmouse.Button_Right bl)
					else true);
			try
				let (i,j) = coord_to_cell x y in
				if (i,j) <> !current_pixel
					then
						(print_mat ren m grid instr;
						if !line
							then bresenham ren mblank i0 j0 i j
							else rectangle_no_writing ren i0 j0 i j;
						refresh ren;
						current_pixel := (i,j))
			with
				| Out_of_screen -> ()
		done
	in

	let rec loop () =
		match Sdlevent.poll_event () with
			(* Quitting *)
			| Some (Window_Event {kind = WindowEvent_Close})
			| Some (KeyDown {keycode = Escape}) -> ()

			(* Resizing *)
			| Some (Window_Event {kind = WindowEvent_Resized wxy}) ->
        (incr cpt_resize;
        if !cpt_resize >= resize_threshold then
					(update_parameters wxy.win_x wxy.win_y;
					draw ();
					cpt_resize := 0);
        loop ())

      | Some (Window_Event {kind = WindowEvent_Exposed})
      | Some Keymap_Changed ->
        (cpt_resize := resize_threshold;
				draw ();
        loop ())

			(* Input *)
			
			| Some (KeyDown {keycode = A}) ->
				(let _ = wait_keyup A in
				line := not !line;
				loop ())
			
			| Some (Mouse_Button_Down {mb_button = mbb; mb_x = x; mb_y = y}) ->
				if mbb = 1 then (* Left-click *)
					(try
						(* Display of the point on which we clicked *)
						let (i0,j0) = coord_to_cell x y in
						ploton ren m i0 j0;
						refresh ren;
						current_pixel := (i0,j0);
						
						(* Looping until the mouse button is released *)
						loop_previs 1;
						let (ifi, jfi) = !current_pixel in

						(* Display of the final line/rectangle *)
						print_mat ren m grid instr;
						(if !line
							then bresenham ren m i0 j0 ifi jfi
							else rectangle ren m i0 j0 ifi jfi);
						refresh ren;
						loop ()
					with
						| Out_of_screen -> loop ())
						
				else if mbb = 3 then (* Right-click *)
					(try
						let (i,j) = coord_to_cell x y in
						plotoff ren m grid i j;
						refresh ren;
						loop ()
					with
						| Out_of_screen -> loop ())

			| _ -> loop ()
	in
	loop ();
	close_graph win;;

(* Interface that lets the user draw from scratch *)
let interface (grid : bool) : bool array array =
	let m = Array.make_matrix 64 128 false in
	let _ = edit grid m in
	m;;
