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
  clear_graph ren;
  (win, ren);;

(** Graphic display **)
let gdraw (ren : Sdlrender.t) : unit =
  print_mat ren gscreen false
    (fun ren -> rect ren (!margin_h-1) (!margin_v-1) (!width+1) (!height+1));;

(* Erases the pixels of the matrix m *)
let wipe (m : bool array array) : unit =
  for j = 0 to 63 do
    for i = 0 to 127 do
      m.(j).(i) <- false
    done;
  done;;

(* Returns only the rectangle, so that sdlrender.fill_rects can be used instead of fill_rect *)
let fast_ploton (i : int) (j : int) : Sdlrect.t =
  Sdlrect.make2 ~pos:(!margin_h + !size*i, !margin_v + !size*j) ~dims:(!size, !size);;

(** Text display **)
(* Older (slower) version *)
(* let old_tdraw (ren : Sdlrender.t) : unit =
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
  refresh ren;; *)

(* Faster tdraw on the model of fast_locate, does one call to Sdlrender.fill_rects *)
(* Displays the tscreen *)
let tdraw (ren : Sdlrender.t) : unit =
  parameters_updated := false;
  let acc = ref [] in
  for j = 0 to 6 do
    for i = 0 to 20 do
      if tscreen.(j).(i) <> "\000" then
        let t = Hashtbl.find repr_text tscreen.(j).(i) in
        (* The visual representation of the character has dimensions 7*5. *)
        for y = 0 to 6 do
          for x = 0 to 4 do
            if t.(5*y+x) then
              let r = fast_ploton (1+6*i+x) (8*j+y) in
              acc := r::!acc
          done
        done
    done
  done;
  clear_graph ren;
  rect ren (!margin_h-1) (!margin_v-1) (!width+1) (!height+1);
  Sdlrender.fill_rects ren (Array.of_list !acc);
  refresh ren;;


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


(* Auxiliary function: skips the first k elements of the list and returns the tail *)
let rec skip_k (k : int) (l : string list) : string list =
  match l with
    | _::t ->
      if k <= 0
        then l
        else skip_k (k-1) t
    | [] -> [];;

(* Same as Locate, but does not draw on the screen *)
let locate_no_refresh (slist : string list) (i : int) (j : int) : unit =
  let n = List.length slist in
  let sl_cut = skip_k (n+i-21) slist in
  List.iteri (fun k s -> tscreen.(j).(i+n-1-k) <- s) sl_cut;;

(* Auxiliary loop to fast_locate:
  for k = bound downto i do (print the character at position k) done *)
let rec fast_locate_aux (ren : Sdlrender.t) (i : int) (j : int) (acc : Sdlrect.t list ref) (l : string list) (k : int) =
  if k >= i then
    match l with
      | s::lt ->
        (tscreen.(j).(k) <- s;
        let t = Hashtbl.find repr_text s in
        for y = 0 to 6 do
          for x = 0 to 4 do
            if t.(5*y+x) then
              let r = fast_ploton (1+6*k+x) (8*j+y) in
              acc := r::!acc
          done
        done;
        fast_locate_aux ren i j acc lt (k-1))
      | [] -> ();;

(* Fast version of Locate, but does not need to refresh the whole screen *)
(* Prints the string s (stored as a list of lexemes in reverse order) in the tscreen at position i,j *)
let locate (ren : Sdlrender.t) (slist : string list) (i : int) (j : int) : unit =
  let n = List.length slist in
  let bound = min 20 (i+n-1) in

  (* White rectangle to cover the area we write in *)
  set_color ren white;
  let white_r = Sdlrect.make2
    ~pos:(!margin_h + !size*(1+6*i), !margin_v + !size*8*j)
    ~dims:(6 * !size * (bound-i+1) - !size, 7 * !size)
  in
  Sdlrender.fill_rect ren white_r;
  set_color ren black;

  (* Display of the characters *)
  let acc = ref [] in
  fast_locate_aux ren i j acc (skip_k (n+i-21) slist) bound;
  Sdlrender.fill_rects ren (Array.of_list !acc);
  refresh ren;;

(* Prints the number z (of type complex) at the right of the writing line *)
(* polar = true if the complex is to be printed in polar form, false in a+ib form *)
let print_number (z : complex) (polar : bool) : unit =
  if z.im = 0.
    then
      (let z_repr = float_to_casio z.re in
      locate_no_refresh (str_to_rev_symblist_simple z_repr) (21-String.length z_repr) !writing_index)
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
              locate_no_refresh a (21-len_a) !writing_index;
              let pos = ref (21-total_length+len_a) in
              line_feed ();
              List.iter
                (fun sl ->
                  locate_no_refresh sl !pos !writing_index;
                  pos := !pos + List.length sl)
                t)
            | [] -> ())
        else
          (let pos = ref (21-total_length) in
          List.iter
            (fun sl ->
              locate_no_refresh sl !pos !writing_index;
              pos := !pos + List.length sl)
            z_repr_l;
          );;

(* Prints the "- DISP -" on the tscreen at line j *)
let print_disp (j : int) : unit =
  locate_no_refresh ["-"; " "; "p"; "s"; "i"; "D"; " "; "-"] 13 j;;

(* Clears the whole tscreen *)
let clear_text () : unit =
  for j = 0 to 6 do
    clear_line j
  done;;

(* Waits for a key to be pressed (or released if getkey_value is 0) *)
(* Raises exception Window_Closed if the window is closed or Escape is pressed *)
(* When the window is resized, if text_graph is true, tdraw is applied, otherwise gdraw *)
let wait_press_key (ren : Sdlrender.t) (text_graph : bool) (getkey_value : int): unit =
  while !getkey <> getkey_value do
    if !parameters_updated
      then
        if text_graph
          then tdraw ren
          else gdraw ren
    else if !exit_key_check
      then raise Window_Closed
  done;;

let wait_press (ren : Sdlrender.t) (text_graph : bool) : unit =
  while !getkey = 0 do
    if !parameters_updated
      then
        if text_graph
          then tdraw ren
          else gdraw ren
    else if !exit_key_check
      then raise Window_Closed
  done;;

(* Waits for Enter key to be pressed *)
let wait_enter (ren : Sdlrender.t) (text_graph : bool) : unit =
  wait_press_key ren text_graph 31;;

(* Wait for a key to be released *)
let wait_release (ren : Sdlrender.t) (text_graph : bool) : unit =
  wait_press_key ren text_graph 0;;