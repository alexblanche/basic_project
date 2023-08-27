(* Graphic functions *)
(* Contains all functions that interact with the graphic window,
  i.e. text mode and graphic mode *)

#use "topfind"
#require "graphics"

#use "picture_editor/picture_drawer.ml"
#use "basic_parsing/basic_encoding.ml"

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

let disp () : unit =
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

(* Test *)
(* open_graphic ();;
let ts =
  [|[|"O"; "k"; ","; " "; "c"; "e"; "c"; "i"; " "; "e"; "s"; "t"; " "; "u";
      "n"; " "; "t"; "e"; "s"; "t"; "."|];
    [|" "; " "; " "; " "; " "; " "; " "; " "; " "; " "; " "; " "; " "; " ";
      " "; " "; " "; " "; " "; " "; " "|];
    [|" "; "A"; "L"; "E"; "X"; "_"; "1"; "1"; "8"; "6"; " "; " "; " "; " ";
      " "; " "; " "; " "; " "; " "; " "|];
    [|" "; " "; " "; " "; " "; " "; " "; " "; " "; " "; " "; " "; " "; " ";
      " "; " "; " "; " "; " "; " "; " "|];
    [|" "; " "; " "; " "; " "; " "; "2"; "0"; "2"; "3"; " "; " "; " "; " ";
      " "; " "; " "; " "; " "; " "; " "|];
    [|" "; " "; " "; " "; " "; " "; " "; " "; " "; " "; " "; " "; " "; " ";
      " "; " "; " "; " "; " "; " "; " "|];
    [|" "; " "; "E"; "v"; "a"; ","; " "; "I"; " "; "l"; "o"; "v"; "e"; " ";
      "y"; "o"; "u"; "!"; " "; " "; " "|];|]
in
for j = 0 to 6 do
  for i = 0 to 20 do
    tscreen.(j).(i) <- ts.(j).(i)
  done;
done;
tdraw ();; *)
