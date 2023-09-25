(* Auxiliary functions for basic_run *)

(* Raised when exiting the program with Esc *)
exception Runtime_interruption;;

(* Assigns the value x to the variable v *)
let assign_var (p : parameters) (x : basic_number) (v : variable) : unit =
  match x,v with
    | Value z, Var vi -> 
      (p.var.(vi) <- z.re;
      p.var.(vi+29) <- z.im)

    | Value z,
      ListIndex (Value a, Arithm [Number (Value i)])
      ->
      (let t = p.list.(int_of_complex a) in
      let iint = int_of_complex i in
      t.(iint) <- z.re;
      t.(iint + (Array.length t)/2) <- z.im)

    | Value z,
      MatIndex (Value a, Arithm [Number (Value i)], Arithm [Number (Value j)])
      ->
      (let m = p.mat.(int_of_complex a) in
      let iint = int_of_complex i in
      let jint = int_of_complex j in
      m.(iint).(jint) <- z.re;
      m.(iint + (Array.length m)/2).(jint) <- z.im)
    
    | _ -> failwith "Runtime error: wrong value in assignment"
  (* to be completed (with lists, matrices) *)



(* Stores the value z in the Ans variable *)
let store_ans (var : float array) (z : complex) : unit =
  (* assign_var p (Value z) (Var 28) *)
  var.(28) <- z.re;
  var.(28+29) <- z.im;;

(* Wait for Enter key to be pressed, then closes the graphic window *)
(* text_graph is true iff the text screen is displayed (as opposed to the graphic screen) *)
let quit (p : parameters) (win : Sdlwindow.t) (ren : Sdlrender.t) (text_graph : bool) : unit =
  (try
    wait_enter p ren text_graph
  with
    | Window_Closed -> ());
  close_graph win;;

(* Prints the value of Ans if val_seen, "Done" otherwise, then quits *)
let quit_print (p : parameters) (win : Sdlwindow.t) (ren : Sdlrender.t) (val_seen : bool) (value : complex) (polar : bool) : unit =
  if val_seen
    then
      (line_feed ();
      print_number value polar;
      tdraw ren)
    else
      (clear_text ();
      locate ren ["e"; "n"; "o"; "D"] 17 0); (* "Done" *)
  quit p win ren true;;


(* Executes the Disp (black triangle) operation *)
let disp (p : parameters) (ren : Sdlrender.t) (writing_index : int ref) : unit =
  line_feed ();
  clear_line !writing_index;
  print_disp !writing_index;
  tdraw ren;
  wait_enter p ren true;
  clear_line !writing_index;
  decr writing_index;
  tdraw ren;; (* Can the last tdraw be removed to speed up the execution? *)