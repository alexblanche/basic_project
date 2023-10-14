(* Auxiliary functions for basic_run *)

(* Raised when exiting the program with Esc *)
exception Runtime_interruption;;

(* Assigns the value x to the variable v *)
let assign_var (p : parameters) (x : entity) (v : variable) : unit =
  match x,v with
    | Value z, Var vi ->
      if vi >= 0 && vi <= 28 then
        (p.var.(vi) <- z.re;
        p.var.(vi+29) <- z.im)
      else failwith "Runtime error: Incorrect index of a variable during assignment"

    | Value z,
      ListIndex (Value a, Complex k)
    | Value z,
      ListIndex (Value a, Arithm [Entity (Value k)])
      ->
      let ai = int_of_complex a in
      let ki = int_of_complex k in
      if ai >= 0 && ai <= 25 then
        (let t = p.list.(ai-1) in
        if ki >= 1 && ki <= (Array.length t)/2 then
          (t.(ki-1) <- z.re;
          t.(ki-1 + (Array.length t)/2) <- z.im)
        else failwith "Runtime error: Incorrect list index during assignment to list index")
      else failwith "Runtime error: Incorrect list during assignment to list index"

    | Value z,
      MatIndex (ai, Complex j, Complex k)
    | Value z,
      MatIndex (ai, Arithm [Entity (Value j)], Arithm [Entity (Value k)])
      ->
      (let m = p.mat.(ai) in
      let ji = int_of_complex j in
      let ki = int_of_complex k in
      if ji >= 1 && ji <= (Array.length m)/2 && ki >= 1 && ki <= Array.length m.(0) then
        (m.(ji-1).(ki-1) <- z.re;
        m.(ji-1 + (Array.length m)/2).(ki-1) <- z.im)
      else failwith "Runtime error: Incorrect matrix index assignment to mat index")
    
    | _ -> failwith "Runtime error: wrong value in assignment"


(* Stores the value z in the Ans variable *)
let store_ans (var : float array) (z : complex) : unit =
  (* assign_var p (Value z) (Var 28) *)
  var.(28) <- z.re;
  var.(57) <- z.im;;

(* Converts as string in reverse string list type to reverse symbol list *)
(* text = true if the string is about to be printed in text mode, and false if in graphic mode *)
let rev_lexlist_to_rev_symblist (lexlist : string list) (text : bool) : string list =
  let l =
    let repr_table =
      if text
        then repr_text
        else repr_graph
    in
    List.fold_left
      (fun acc lex ->
        if Hashtbl.mem repr_table lex
          then lex::acc
          else
            let repr =
              try
                List.assoc lex text_display
              with
                | Not_found -> (print_endline ("Runtime warning: Unknown lexeme "^lex); "")
            in
            List.rev_append (str_to_rev_symblist_full repr) acc)
      []
      lexlist
  in
  List.rev l;;

(* Wait for Enter key to be pressed, then closes the graphic window *)
(* text_screen is true iff the text screen is displayed (as opposed to the graphic screen) *)
let quit (win : Sdlwindow.t) (ren : Sdlrender.t) (text_screen : bool) : unit =
  (try
    wait_release ren text_screen;
    wait_enter ren text_screen
  with
    | Window_Closed -> ());
  close_graph win;;

(* Prints the value of Ans if val_seen, "Done" otherwise, then quits *)
let quit_print (win : Sdlwindow.t) (ren : Sdlrender.t) (val_seen : bool) (value : complex) (polar : bool) (string_seen : bool) (text_screen : bool) : unit =
  if not text_screen then
    (wait_release ren false;
    wait_enter ren false;
    line_feed ();
    clear_line !writing_index;
    locate_no_refresh ["e"; "n"; "o"; "D"] 17 !writing_index;
    tdraw ren)
  else if val_seen then
    (line_feed ();
    print_number value polar;
    tdraw ren)
  else if not string_seen then
    (clear_text ();
    locate ren ["e"; "n"; "o"; "D"] 17 0); (* "Done" *)
  quit win ren true;;


(* Executes the Disp (black triangle) operation *)
let disp (p : parameters) (ren : Sdlrender.t) (writing_index : int ref) : unit =
  line_feed ();
  clear_line !writing_index;
  print_disp !writing_index;
  tdraw ren;
  wait_release ren true;
  wait_enter ren true;
  clear_line !writing_index;
  decr writing_index;
  tdraw ren;; (* Can the last tdraw be removed to speed up the execution? *)

(* Handles the display of strings of length 21 < len < 21*7 symbols *)
let display_long_string (sl : string list) : unit =
  let rec aux sl =
    match split_k sl 21 with
      | [], _ -> ()
      | slk, [] ->
        (locate_no_refresh slk 0 !writing_index;
        if List.length slk = 21
          then line_feed ())
      | slk, slnk ->
        (locate_no_refresh slk 0 !writing_index;
        line_feed ();
        aux slnk)
  in
  let rsl = List.rev sl in
  aux rsl;;

(* Handles the display of strings of length len >= 21*7 symbols *)
let display_extra_long_string (sl : string list) : unit =
  let rec aux sl =
    match split_k sl 21 with
      | [], _ -> ()
      | slk, [] ->
        locate_no_refresh slk 0 !writing_index
      | slk, slnk ->
        (locate_no_refresh slk 0 !writing_index;
        line_feed ();
        aux slnk)
  in
  let rsl = List.rev sl in
  aux rsl;;
