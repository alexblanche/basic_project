(* Execution of Basic code *)


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
let quit (win : Sdlwindow.t) (ren : Sdlrender.t) (text_graph : bool) : unit =
  (try
    wait_enter ren text_graph
  with
    | Window_Closed -> ());
  close_graph win;;

(* Prints the value of Ans if val_seen, "Done" otherwise, then quits *)
let quit_print (win : Sdlwindow.t) (ren : Sdlrender.t) (val_seen : bool) (value : complex) (polar : bool) : unit =
  if val_seen
    then
      (line_feed ();
      print_number value polar)
    else
      (clear_text ();
      locate ["e"; "n"; "o"; "D"] 17 0); (* "Done" *)
  tdraw ren;
  quit win ren true;;


(* Executes the Disp (black triangle) operation *)
let disp (ren : Sdlrender.t) (writing_index : int ref) : unit =
  line_feed ();
  clear_line !writing_index;
  print_disp !writing_index;
  tdraw ren;
  wait_enter ren true;
  clear_line !writing_index;
  decr writing_index;
  tdraw ren;; (* Can the last tdraw be removed to speed up the execution? *)

(* Executes the ? (QMark) operation *)
let qmark (win : Sdlwindow.t) (ren : Sdlrender.t) (p : parameters) (v : variable) : unit =
  (* Same as wait_keydown but supports resizing *)
  let rec wait_keydown_resize (cpt_resize : int) : Sdlkeycode.t =
    match Sdlevent.poll_event () with
      (* Quitting *)
      | Some (Window_Event {kind = WindowEvent_Close}) -> raise Window_Closed
      (* Resizing *)
			| Some (Window_Event {kind = WindowEvent_Resized wxy}) ->
        if cpt_resize >= resize_threshold then
					(update_parameters wxy.win_x wxy.win_y;
					tdraw ren;
          wait_keydown_resize 0)
        else wait_keydown_resize (cpt_resize + 1)
      
      | Some (KeyDown {keycode = keycode}) -> keycode
      | _ -> wait_keydown_resize cpt_resize
  in

  (* Main loop *)
  (* ns: list of strings storing the number entered
    x: index of writing in line !writing_index *)
  let rec loop (ns : string list) (x : int) : string list =
    let key = wait_keydown_resize 0 in
    flush_events ();
    (* Enter or keypad Enter *)
    if (key = Return || key = KP_Enter) && ns <> []
      then List.rev ns
    else if key = Escape
      then
        if ns <> []
          then (* Something was typed: Reset *)
            (clear_text ();
            locate ["?"] 0 0;
            writing_index := 1;
            tdraw ren;
            wait_keyup key;
            loop [] 0)
          else (* Nothing was typed: Exitting the program *)
            raise Runtime_interruption
    else if List.mem key (* 0..9, '.': the symbol is the lexeme *)
        [KP_0; KP_1; KP_2; KP_3; KP_4; KP_5; KP_6; KP_7; KP_8; KP_9; KP_Period]
      then
        let cs = List.assoc key
          [(KP_0, "0"); (KP_1, "1"); (KP_2, "2"); (KP_3, "3"); (KP_4, "4");
          (KP_5, "5"); (KP_6, "6"); (KP_7, "7"); (KP_8, "8"); (KP_9, "9");
          (KP_Period, ".")]
        in
        if x = 21 then
          (line_feed ();
          locate [cs] 0 !writing_index;
          tdraw ren;
          wait_keyup key;
          loop (cs::ns) 1)
        else
          (locate [cs] x !writing_index;
          tdraw ren;
          wait_keyup key;
          loop (cs::ns) (x+1))
    else if List.mem key (* Other lexemes: their representation is different from the lexeme *)
        [LeftParen; RightParen; KP_Plus; KP_Minus; KP_Multiply; KP_Divide]
      then
        let lex = List.assoc key
          [(LeftParen, "LPAR"); (RightParen, "RPAR");
          (KP_Plus, "PLUS"); (KP_Minus, "MINUS"); (KP_Multiply, "TIMES"); (KP_Divide, "DIVIDED")]
        in
        let repr = List.assoc lex text_display in (* String representing the function *)
        let cs = str_to_rev_symblist_full repr in (* string list printed by the Locate *)
        let len = List.length cs in
        if x + len >= 21 then
          (let (csk,csnk) = split_k cs (len-(21-x)) in
          locate (List.rev csnk) x !writing_index;
          line_feed ();
          locate (List.rev csk) 0 !writing_index;
          tdraw ren;
          wait_keyup key;
          loop (lex::ns) ((x + len) mod 21))
        else
          (locate cs x !writing_index;
          tdraw ren;
          wait_keyup key;
          loop (lex::ns) (x+len))
    else
      loop ns x
  in

  line_feed ();
  clear_line !writing_index;
  locate ["?"] 0 !writing_index;
  line_feed ();
  tdraw ren;
  (* The writing line is not cleared on screen until a character is typed *)
  clear_line !writing_index;

  let ns = loop [] 0 in
  
  let (e,t) = extract_expr (List.rev ns) in
  if t <> []
    then failwith "Runtime error: wrong entry";
  let z = eval p e in
  assign_var p (Value z) v;;

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

    (* Checks whether the window was resized/closed *)
    (* It has minimal cost, but disturbs Getkey. *)
    let found_keyup = check_resize_close () in
    let _ = (* If a KeyUp event was encountered, refresh getkey value *)
      if !getkey_encountered && found_keyup
        then
          match read_getkey_input 0 with
            | (_, Some keycode) ->
              p.getkey <- get_getkey_val keycode
            | _ -> p.getkey <- 0
    in

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
            quit win ren true (* Quit after the string *))
          else if i<n-1 && code.(i+1) = Disp
            then
              (disp ren writing_index;
              aux (i+2))
            else aux (i+1))
          
      | Locate (e1, e2, c) ->
        let z1 = eval p e1 in
        let z2 = eval p e2 in
        (if not
          ((is_int z1) && (is_int z2)
          && (z1.re >= 1.) && (z1.re <= 21.)
          && (z2.re >= 1.) && (z2.re <= 7.))
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
            quit win ren true (* Quit after the string *))
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
        try
          let j = List.assoc name proglist in
          aux j
        with
          | Not_found -> failwith ("Runtime error: Prog "^name^" not found"))

      | End ->
        (match !prog_goback with
          | i::t ->
            (prog_goback := t;
            aux i)
          | [] -> quit_print win ren !val_seen !last_val p.polar)

      | _ -> failwith ("Runtime error: unexpected command at line "^(string_of_int i))
  in
  
  (try
    aux 0
  with
    | Runtime_interruption
    | Window_Closed -> print_endline "--- Runtime interruption ---"
    | Failure s -> print_endline s);
  close_graph win;;

(* To do:
  - Slow down execution
    An empty for loop executes 798 rounds in 1s,
    for specific functions (mainly display), measurements are needed *)