(* Graphic functions *)
(* Contains all functions that interact with the graphic window,
  i.e. text mode and graphic mode *)

(* #use "topfind"
#require "graphics"

#use "picture_editor/picture_drawer.ml"
#use "basic_parsing/basic_encoding.ml" *)

(* repr_text, repr_graph: hash tables of the visual representation of the characters in
  text mode and graphic mode respectively *)
let repr_text = visual ();;
let repr_graph = visualgraph ();;

(* tscreen: content of the text screen *)
let tscreen = Array.make_matrix 7 21 " ";;
(* writing_index: line index the write at *)
let writing_index = ref 0;;

(* gscreen: content of the graphic screen *)
let gscreen = Array.make_matrix 64 128 false;;

let open_graphic () : unit =
  open_graph "";
	resize_window (2*margin + width) (2*margin + height);
	set_window_title "Basic Emulator";
	cache ();
	sync ();;

let wait_enter () : unit =
  (* '\013' = ENTER *)
  while not (read_key () = '\013') do
    ()
  done;;

(* Erases the pixels of the matrix m *)
let wipe (m : bool array array) : unit =
  for j = 0 to 63 do
    for i = 0 to 127 do
      m.(j).(i) <- false
    done;
  done;;

(* Graphic display *)
let gdraw () : unit =
  print_mat gscreen false (fun () -> rect margin margin width height);;

(* Text display *)
let tdraw () : unit =
  clear_graph ();
  rect margin margin width height;
  let m = Array.make_matrix 64 128 false in
  for j = 0 to 6 do
    for i = 0 to 20 do
      let t = Hashtbl.find repr_text tscreen.(j).(i) in
      (* The visual representation of the character has dimensions 7*5. *)
      for y = 0 to 6 do
        for x = 0 to 4 do
          if t.(5*y+x)
            then ploton m (1+6*i+x) (63-8*j-y)
        done;
      done;
    done;
  done;
  sync ();;
(* Is it useful to have a bool matrix for the text screen too?
  If tdraw is too slow, I will consider it. *)


(* Prints the string s (stored as a list of lexemes) in the tscreen at position i,j *)
let locate (slist : string list) (i : int) (j : int) : unit =
  List.iteri (fun k s -> if i + k <= 20 then tscreen.(j).(i+k) <- s) slist;;

(* Prints the number z (of type complex) at the right of the writing line *)
(* polar = true if the complex is to be printed in polar form, false in a+ib form *)
let print_number (z : complex) (polar : bool) : unit =
  if z.im = 0.
    then
      (let z_repr = float_to_casio z.re in
      locate (string_to_lexlist z_repr) (21-String.length z_repr) !writing_index;
      incr writing_index)
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
              List.iter
                (fun sl ->
                  locate sl !pos (!writing_index+1);
                  pos := !pos + List.length sl)
                t;
              writing_index := !writing_index + 2)
            | [] -> ())
        else
          (let pos = ref (21-total_length) in
          List.iter
            (fun sl ->
              locate sl !pos !writing_index;
              pos := !pos + List.length sl)
            z_repr_l;
          incr writing_index);;

(* Prints the "- DISP -" on the tscreen at line j *)
let print_disp (j : int) : unit =
  locate ["-"; " "; "D"; "i"; "s"; "p"; " "; "-"] 13 j;;

(* Clears line j of the tscreen *)
let clear_line (j : int) : unit =
  tscreen.(j) <- Array.make 21 " ";;

(* Clears the whole tscreen *)
let clear_text () : unit =
  for j = 0 to 6 do
    clear_line j
  done;;

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
