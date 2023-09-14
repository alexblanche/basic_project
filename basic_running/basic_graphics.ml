(* Graphic functions *)
(* Contains all functions that interact with the graphic window,
  i.e. text mode and graphic mode *)

(* repr_text, repr_graph: hash tables of the visual representation of the characters in
  text mode and graphic mode respectively *)
let repr_text = visual ();;
let repr_graph = visualgraph ();;

(* tscreen: content of the text screen *)
let tscreen = Array.make_matrix 7 21 "\000";;
(* writing_index: line index where the last character was written (-1 by default) *)
let writing_index = ref (-1);;

(* gscreen: content of the graphic screen *)
let gscreen = Array.make_matrix 64 128 false;;

(* Opens the graphic window and returns it along with its render *)
let open_graphic () : Sdlwindow.t * Sdlrender.t =
  let (win, ren) = config false (fun _ -> ()) in
  Sdlwindow.set_title ~window:win ~title:"Basic Emulator";
  refresh ren;
  (win, ren);;

(** Graphic display **)
let gdraw (ren : Sdlrender.t) : unit =
  print_mat ren gscreen false
    (fun ren -> rect ren !margin_h !margin_v !width !height);;

(* Erases the pixels of the matrix m *)
let wipe (m : bool array array) : unit =
  for j = 0 to 63 do
    for i = 0 to 127 do
      m.(j).(i) <- false
    done;
  done;;

(** Text display **)
let tdraw (ren : Sdlrender.t) : unit =
  clear_graph ren;
  rect ren !margin_h !margin_v !width !height;
  let m = Array.make_matrix 64 128 false in
  for j = 0 to 6 do
    for i = 0 to 20 do
      if tscreen.(j).(i) <> "\000" then
        let t = Hashtbl.find repr_text tscreen.(j).(i) in
        (* The visual representation of the character has dimensions 7*5. *)
        for y = 0 to 6 do
          for x = 0 to 4 do
            if t.(5*y+x)
              then ploton ren m (1+6*i+x) (8*j+y)
          done
        done
    done
  done;
  refresh ren;;
(* Is it useful to have a bool matrix for the text screen too?
  If tdraw is too slow, I will consider it. *)

(* Clears line j of the tscreen *)
let clear_line (j : int) : unit =
  tscreen.(j) <- Array.make 21 "\000";;

(* Scrolls the tscreen by one line *)
let scroll () : unit =
  for j = 0 to 5 do
    tscreen.(j) <- tscreen.(j+1)
  done;
  clear_line 6;;

(* Increases writing_index by 1
  If it reaches 7, the tscreeen scrolls and writing_index is set back at 6. *)
let line_feed () : unit =
  incr writing_index;
  if !writing_index = 7 then
    (scroll ();
    decr writing_index);;

(* Prints the string s (stored as a list of lexemes in reverse order) in the tscreen at position i,j *)
let locate (slist : string list) (i : int) (j : int) : unit =
  let n = List.length slist in
  List.iteri (fun k s -> if i+n <= 21+k then tscreen.(j).(i+n-1-k) <- s) slist;;

(* Prints the number z (of type complex) at the right of the writing line *)
(* polar = true if the complex is to be printed in polar form, false in a+ib form *)
let print_number (z : complex) (polar : bool) : unit =
  if z.im = 0.
    then
      (let z_repr = float_to_casio z.re in
      locate (str_to_rev_symblist_simple z_repr) (21-String.length z_repr) !writing_index)
    else
      (* if the total length is too long, cut after the real part *)
      let z_repr_l =
        if polar
          then complex_to_casio_polar z
          else complex_to_casio_aib z
      in
      let total_length = List.fold_left (fun s l -> s+List.length l) 0 z_repr_l in
      if total_length > 20
        then
          (match z_repr_l with
            | a::t ->
              (let len_a = List.length a in
              locate a (21-len_a) !writing_index;
              let pos = ref (21-total_length+len_a) in
              line_feed ();
              List.iter
                (fun sl ->
                  locate sl !pos !writing_index;
                  pos := !pos + List.length sl)
                t)
            | [] -> ())
        else
          (let pos = ref (21-total_length) in
          List.iter
            (fun sl ->
              locate sl !pos !writing_index;
              pos := !pos + List.length sl)
            z_repr_l;
          );;

(* Prints the "- DISP -" on the tscreen at line j *)
let print_disp (j : int) : unit =
  locate ["-"; " "; "p"; "s"; "i"; "D"; " "; "-"] 13 j;;

(* Clears the whole tscreen *)
let clear_text () : unit =
  for j = 0 to 6 do
    clear_line j
  done;;

(* Waits for Enter key to be pressed *)
(* Raises exception Window_Closed if the window is closed or Escape is pressed *)
(* When the window is resized, if text_graph is true, tdraw is applied, otherwise gdraw *)
let wait_enter (ren : Sdlrender.t) (text_graph : bool) : unit =
  let rec aux (cpt_resize : int) : unit =
    match Sdlevent.poll_event () with
      (* Quitting *)
      | Some (Window_Event {kind = WindowEvent_Close}) -> raise Window_Closed

      (* Resizing *)
			| Some (Window_Event {kind = WindowEvent_Resized wxy}) ->
        if cpt_resize >= resize_threshold then
					(update_parameters wxy.win_x wxy.win_y;
					if text_graph then tdraw ren else gdraw ren;
          aux 0)
        else aux (cpt_resize + 1)

      | Some (Window_Event {kind = WindowEvent_Exposed})
      | Some Keymap_Changed -> aux resize_threshold

      (* Input *)
      | Some (KeyDown {keycode = k}) ->
        if k = Return || k = KP_Enter (* Enter or keypad Enter *)
          then ()
        else if k = Escape
          then raise Window_Closed
        else aux cpt_resize

      | _ -> aux cpt_resize
  in
  aux 0;;