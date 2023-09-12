(* Execution of Basic code *)

(* #use "basic_parsing/basic_type.ml"
#use "basic_parsing/basic_encoding.ml" *)
(* #use "basic_running/arithmetic_parsing.ml"
#use "basic_running/graphic.ml" *)

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
let quit (win : Sdlwindow.t) : unit =
  wait_enter ();
  close_graph win;
  Sdl.quit ();;

(* Prints the value of Ans if val_seen, "Done" otherwise, then quits *)
let quit_print (win : Sdlwindow.t) (ren : Sdlrender.t) (val_seen : bool) (value : complex) (polar : bool) : unit =
  if val_seen
    then
      (line_feed ();
      print_number value polar)
    else
      (clear_text ();
      locate ["D"; "o"; "n"; "e"] 17 0);
  tdraw ren;
  quit win;;


(* Executes the Disp (black triangle) operation *)
let disp (ren : Sdlrender.t) (writing_index : int ref) : unit =
  line_feed ();
  clear_line !writing_index;
  print_disp !writing_index;
  tdraw ren;
  wait_enter ();
  clear_line !writing_index;
  decr writing_index;
  tdraw ren;; (* Can the last tdraw be removed to speed up the execution? *)

(* When the list l has length n > k, then the function returns two lists:
  the first k elements of l, then the other n-k *)
let split_k (l : 'a list) (k : int) =
  let rec aux a b l i =
    match l with
      | h::t ->
        if i<k
          then aux (h::a) b t (i+1)
          else aux a (h::b) t (i+1)
      | [] -> (a,b)
  in
  aux [] [] l 0;; 

(* Executes the ? (QMark) operation *)
let qmark (win : Sdlwindow.t) (ren : Sdlrender.t) (p : parameters) (v : variable) : unit =
  line_feed ();
  clear_line !writing_index;
  locate ["?"] 0 !writing_index;
  line_feed ();
  tdraw ren;
  clear_line !writing_index;
  let exit = ref false in
  let ns = ref [] in (* list of strings storing the number entered *)
  let x = ref 0 in (* Index of writing in line !writing_index *)
  while not !exit do
    let {mouse_x; mouse_y; button; keypressed; key} =
			wait_next_event [(* Button_down; *) Key_pressed]
		in
		exit := (key = '\013' && !ns <> []); (* Enter *)

    if key >= '0' && key <= '9' || key = '.' (* || function... *) then
      (let cs = String.make 1 key in
      
      (* (* To do *)
      let cs = List.assoc (String.make 1 key) in
      let lexcs = str_to_rev_symblist_full cs in
      ns := List.rev_append lexcs !ns in
      (*...*)
      let len = List.length lexcs in
      if !x + len >= 21 then
        (let (csk,csn) = split_k lexcs in
        locate csk !x !writing_index;
        line_feed ();
        locate csn 0 !writing_index)
      else locate lexcs !x !writing_index;
      x := (!x + len) mod 21;
      *)

      ns := cs::!ns;
      if !x = 21 then
        (x := 0;
        line_feed ());
      locate [cs] !x !writing_index;
      incr x;
      tdraw ren)
    else if key = '\027' && !ns <> [] then (* Esc and something was entered: reset *)
      (ns := [];
      x := 0;
      clear_text ();
      locate ["?"] 0 0;
      writing_index := 1;
      tdraw ren)
    else if key = '\027' then (* Esc and nothing was entered: quit *)
      (close_graph win;
      Sdl.quit ();
      raise Runtime_interruption)
  done;
  let (e,t) = extract_expr (List.rev !ns) in
  if t <> []
    then failwith "Runtime error: wrong entry";
  let z = eval p e in
  assign_var p (Value z) v;;

(* Getkey values *)
(* Returns the Getkey value of the given key, as observerd on a Casio calculator *)
(* Value = 10*a + b
b\a  7     6    5    4    3    2
9    F1    F2   F3   F4   F5   F6
8    SHIFT OPTN VARS MENU <    ^
7    ALPHA x2   ^    EXIT v    >
6    XthT  log  ln   sin  cos  tan
5    FRAC  FD   (    )    ,    ->
4    7     8    9    DEL
3    4     5    6    x     /
2    1     2    3    +     -
1    0     .    E    (-)   EXE
*)
let get_getkey_val (key : char) : int =
  try
    List.assoc key
    (* (in Azerty)
      EXE = Enter
      Arrows = Z Q S D
      + - x div . , = + - * / . ,
      0-9 : 0-9
      F1-F6 = & é quote ' ( -  (i.e. 1-6)
      SHIFT, ALPHA = ^2 tab
      DEL = Backspace
      (AC/on = Delete, stops the program, not available in Getkey)
    *)
    (* To do: toggle Qwerty mode *)
    [
      (* No key *)
      ('\000', 0);

      ('\028', 79); ('\233', 69);   ('\034', 59);   ('\039', 49);   ('\040', 39); ('\045', 29);
      ('\178', 78); (* ('\000', 68); ('\000', 58);  ('\000', 48); *) ('q', 38);   ('z', 28);
      ('\009', 77); (* ('\000', 67); ('\000', 57); *) ('\127', 47); ('s', 37);    ('d', 27);
      (* ('\000', 76); ('\000', 66);   ('\000', 56);   ('\000', 46);   ('\000', 36); ('\000', 26); *)
      (* ('\000', 75); ('\000', 65);   ('\000', 55);   ('\000', 45); *)  (',', 35);  (* ('\000', 25); *)
      ('7', 74);    ('8', 64);      ('9', 54);      ('\008', 44);
      ('4', 73);    ('5', 63);      ('6', 53);      ('*', 43);      ('/', 33);
      ('1', 72);    ('2', 62);      ('3', 52);      ('+', 42);      ('-', 32);
      ('0', 71);    ('.', 61);    (*  ('\000', 51);   ('\000', 41); *)  ('\013', 31);

      (* Caps-lock alternatives *)
      ('Q', 38); ('Z', 28); ('S', 37); ('D', 27);
      ('\201', 69) (* Uppercase é = 2 *)
    ]
  with
    | Not_found -> 0;;


(* General execution function *)
let run (proj : project_content) ((code, proglist): basic_code) : unit =
  
  (** Initialization of all parameters **)
  let (p : parameters) = new_param proj in

  let n = Array.length code in
  
  (* prog_goback: pile of indices to return to when the end of a program is reached *)
  let prog_goback = ref [] in

  (* for_info: pile containing for each open For loop, the tuple (vari, iback, asc, vto, vstep)
    where:
    - vari: index of the variable involved in the For loop
    - iback: the index where to go back after the Next if the loop is not finished
    - asc: a boolean that indicates if the For loop is ascending (the step value is > 0) or descending (step < 0)
    - vto, vstep (float): respectively the "To" value, compared to var in the loop condition, and the "Step" value *)
  let for_info = ref [] in

  (* Last value seen, is printed at the end of the execution *)
  let last_val = ref ({re = 0.; im = 0.} : complex) in
  (* val_seen: to decide whether to print "Done" or last_val at the end of the execution *)
  let val_seen = ref false in

  (* Initialization of the graphic window and graphic parameters *)
  let (win, ren) = open_graphic () in
  set_color ren black;
  clear_text ();
  wipe gscreen;
  writing_index := -1;

  (** Looping function **)
  let rec aux (i : int) : unit =
    
    if i >= n then (* End of the execution *)
      quit_print win ren !val_seen !last_val p.polar
    else

    (* Check of key pressed *)
    (* wait_next_event with Poll does not work,
      because the key presses are queued and take forever to dequeue.
      The current method is satisfyingly responsive. *)
    (if Graphics.key_pressed ()
      then
        let k = Graphics.read_key () in
        if k = '\027' (* Esc *)
          then (close_graph win; raise Runtime_interruption)
        else
          p.getkey <- get_getkey_val k
      (* else p.getkey <- 0*));
    (* Big problem: impossible to know when a key is released! :( *)
    (* To do: use SDL2 with OCamlSDL2 *) 

    match code.(i) with

      | Goto j -> aux j

      | If (e,j) ->
        if is_not_zero (eval p e)
          then aux (i+1)
          else aux j

      | JumpIf (e,j) ->
        if is_not_zero (eval p e)
          then aux j
          else aux (i+1)
          
      | Expr (Arithm al) ->
        let z = eval p (Arithm al) in
        (store_ans p.var z;
        last_val := z;
        val_seen := true;
        if i<n-1 && code.(i+1) = Disp
          then
            (line_feed ();
            print_number z p.polar;
            disp ren writing_index;
            aux (i+2))
          else aux (i+1))
          
      | Assign (QMark, v) ->
        (qmark win ren p v;
        aux (i+1))

      | Assign (e, v) ->
        let z = eval p e in
        (last_val := z;
        val_seen := true;
        (match v with
          | Var vi ->
            (* (p.var.(vi) <- z.re;
            p.var.(vi+29) <- z.im) *)
            assign_var p (Value z) v

          | ListIndex (a, iexp) ->
            (let vala = get_val p a in
            let ie = eval p iexp in
            if not (is_int vala && is_int ie)
              then failwith "Runtime error: wrong index for list";
            (* let t = p.list.(int_of_complex vala) in
            let iint = int_of_complex ie in
            t.(iint) <- z.re;
            t.(iint + (Array.length t)/2) <- z.im) *)
            assign_var p (Value z) (ListIndex (Value vala, Arithm [Number (Value ie)])))

          | MatIndex (a, iexp, jexp) ->
            (let vala = get_val p a in
            let ie = eval p iexp in
            let je = eval p jexp in
            if not (is_int vala && is_int ie && is_int je)
              then failwith "Runtime error: wrong index for matrix";
            (* let m = p.mat.(int_of_complex vala) in
            let iint = int_of_complex ie in
            let jint = int_of_complex je in
            m.(iint).(jint) <- z.re;
            m.(iint + (Array.length m)/2).(jint) <- z.im) *)
            assign_var p (Value z)
              (MatIndex (Value vala,
                Arithm [Number (Value ie)], Arithm [Number (Value je)])))

          | _ -> aux (i+1) (* temporary *)
        );
        if i<n-1 && code.(i+1) = Disp
          then
            (line_feed ();
            print_number z p.polar;
            disp ren writing_index;
            aux (i+2))
          else aux (i+1))
          (* 
          | Getkey
          | Random) *) (* to do *)
      
      | String sl ->
        (line_feed ();
        clear_line !writing_index;
        locate sl 0 !writing_index;
        tdraw ren;
        val_seen := true;
        if (i = n-2 && code.(i+1) = End
          || i = n-3 && code.(i+1) = Disp && code.(i+2) = End)
          && !prog_goback = []
          then (* End of the program *)
            (if code.(i+1) = Disp
              then disp ren writing_index;
            quit win (* Quit after the string *))
          else if i<n-1 && code.(i+1) = Disp
            then
              (disp ren writing_index;
              aux (i+2))
            else aux (i+1))
          
      | Locate (e1, e2, c) ->
        let z1 = eval p e1 in
        let z2 = eval p e2 in
        (if not
          ((is_int z1)
          && (is_int z2)
          && (z1.re >= 1.)
          && (z1.re <= 21.)
          && (z2.re >= 1.)
          && (z2.re <= 7.))
          then failwith "Runtime error: wrong arguments for Locate";
        (* The coordinates in Casio Basic are between 1 and 21 *)
        let sl =
          match c with
            | Loc_text s -> s
            | Loc_expr e ->
              (let z = eval p e in
              if z.im <> 0. (* In Casio Basic, Locate does not handle complex numbers *)
                then failwith "Runtime error: a number printed by Locate cannot be a complex number";
              let z_repr = float_to_casio z.re in
              str_to_rev_symblist_simple z_repr)
        in
        locate sl ((int_of_complex z1)-1) ((int_of_complex z2)-1);
        tdraw ren;
        val_seen := true;
        if (i = n-2 && code.(i+1) = End
          || i = n-3 && code.(i+1) = Disp && code.(i+2) = End)
          && !prog_goback = []
          then (* End of the program *)
            (if code.(i+1) = Disp
              then disp ren writing_index;
            quit win (* Quit after the string *))
          else if i<n-1 && code.(i+1) = Disp
          then
            (disp ren writing_index;
            aux (i+2))
          else aux (i+1))

      | Function "CLRTEXT" ->
        (clear_text ();
        writing_index := -1;
        val_seen := false;
        aux (i+1))
      
      | For (vi, e1, e2, e3, inext) ->
        (let z1 = eval p e1 in
        if z1.im <> 0.
          then failwith "Runtime error: complex value given to a For loop";
        p.var.(vi) <- z1.re;
        p.var.(vi+29) <- 0.;
        let z2 = eval p e2 in
        let z3 = eval p e3 in
        if z2.im <> 0. || z3.im <> 0.
          then failwith "Runtime error: complex value given to a For loop"
        else if is_zero z3
          then failwith "Runtime error: the step value of a For loop is 0";
          
        let asc = z3.re > 0. in
        for_info := (vi, i+1, asc, z2.re, z3.re)::!for_info;

        (* First test of the loop condition *)
        if (asc && p.var.(vi) <= z2.re) || ((not asc) && p.var.(vi) >= z2.re)
          then aux (i+1)
          else
            (for_info := List.tl !for_info;
            aux inext) (* The loop is finished before starting *)
        )
      
      | Next ->
        (match !for_info with
          | (vari, iback, asc, vto, vstep)::_ ->
            (p.var.(vari) <- p.var.(vari) +. vstep;
            if (asc && p.var.(vari) <= vto) || ((not asc) && p.var.(vari) >= vto)
              then aux iback
              else (* Exitting the loop *)
                (for_info := List.tl !for_info;
                aux (i+1)))
          | [] -> failwith "Runtime error: unexpected Next")
      
      | Prog name ->
        (prog_goback := (i+1)::!prog_goback;
        let j = List.assoc name proglist in
        aux j)

      | End ->
        (match !prog_goback with
          | i::t ->
            (prog_goback := t;
            aux i)
          | [] -> quit_print win ren !val_seen !last_val p.polar)

      | _ -> failwith ("Runtime error: unexpected command at line "^(string_of_int i))
  in
  
  aux 0;;

(* To do:
  - Slow down execution
    An empty for loop executes 798 rounds in 1s,
    for specific functions (mainly display), measurements are needed *)