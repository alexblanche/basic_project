(* Execution of Basic code *)

(* #use "basic_parsing/basic_type.ml"
#use "basic_parsing/basic_encoding.ml" *)
(* #use "basic_running/arithmetic_parsing.ml"
#use "basic_running/graphic.ml" *)

(* Executes the Disp (black triangle) operation *)
let disp (writing_index : int ref) : unit =
  if !writing_index = 7
    then (scroll (); decr writing_index);
  clear_line !writing_index;
  print_disp !writing_index;
  tdraw ();
  wait_enter ();
  clear_line !writing_index;
  tdraw ();; (* Can the last tdraw () be removed to speed up the execution? *)

(* Stores the value z in the Ans variable *)
let store_ans (var : float array) (z : complex) : unit =
  var.(28) <- z.re;
  var.(28+29) <- z.im;;

(* Wait for Enter key to be pressed, then closes the graphic window *)
let quit () : unit =
  wait_enter ();
  close_graph ();;

(* Prints the value of Ans, then quits *)
let quit_print (val_seen : bool) (value : complex) (polar : bool) : unit =
  if val_seen
    then print_number value polar
    else
      (clear_text ();
      locate ["D"; "o"; "n"; "e"] 17 0);
  tdraw ();
  quit ();;


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
  open_graphic ();
  set_color black;
  clear_text ();
  wipe gscreen;
  writing_index := 0;

  (** Looping function **)
  let rec aux (i : int) : unit =
    if i >= n then (* End of the execution *)
      quit_print !val_seen !last_val p.polar
    else

    match code.(i) with

      | Goto j -> aux j

      | If (e,j) ->
        if is_not_zero (eval p e)
          then aux (i+1)
          else aux j
          
      | Expr (Arithm al) ->
        let z = eval p (Arithm al) in
        (store_ans p.var z;
        last_val := z;
        val_seen := true;
        if i<n-1 && code.(i+1) = Disp
          then
            (print_number z p.polar;
            disp writing_index;
            aux (i+2))
          else aux (i+1))
          
      | Assign (e, v) ->
        let z = eval p e in
        (last_val := z;
        val_seen := true;
        (match v with
          | Var vi ->
            (p.var.(vi) <- z.re;
            p.var.(vi+29) <- z.im)

          | ListIndex (a, iexp) ->
            (let vala = get_val p a in
            let ie = eval p iexp in
            if not (is_int vala && is_int ie)
              then failwith "Runtime error: wrong index for list";
            let t = p.list.(int_of_complex vala) in
            let iint = int_of_complex ie in
            t.(iint) <- z.re;
            t.(iint + (Array.length t)/2) <- z.im)

          | MatIndex (a, iexp, jexp) ->
            (let vala = get_val p a in
            let ie = eval p iexp in
            let je = eval p jexp in
            if not (is_int vala && is_int ie && is_int je)
              then failwith "Runtime error: wrong index for matrix";
            let m = p.mat.(int_of_complex vala) in
            let iint = int_of_complex ie in
            let jint = int_of_complex je in
            m.(iint).(jint) <- z.re;
            m.(iint + (Array.length m)/2).(jint) <- z.im)
          | _ -> aux (i+1) (* temporary *)
        );
        if i<n-1 && code.(i+1) = Disp
          then
            (print_number z p.polar;
            disp writing_index;
            aux (i+2))
          else aux (i+1))
          (* 
          | Getkey
          | Random) *) (* to do *)
      
      | String sl ->
        (if !writing_index = 7
          then (scroll (); decr writing_index);
        clear_line !writing_index;
        locate sl 0 !writing_index;
        incr writing_index;
        tdraw ();
        if (i = n-2 && code.(i+1) = End
          || i = n-3 && code.(i+1) = Disp && code.(i+2) = End)
          && !prog_goback = []
          then (* End of the program *)
            (if code.(i+1) = Disp
              then disp writing_index;
            quit () (* Quit after the string *))
          else if i<n-1 && code.(i+1) = Disp
            then
              (disp writing_index;
              aux (i+2))
            else aux (i+1))
          
      | Locate (e1, e2, sl) ->
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
        locate sl (int_of_complex z1) (int_of_complex z2);
        if i<n-1 && code.(i+1) = Disp
          then
            (disp writing_index;
            aux (i+2))
          else aux (i+1))

      | Function "CLRTEXT" ->
        (clear_text ();
        writing_index := 0;
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
        else if z3.im = 0.
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
          | [] -> quit_print !val_seen !last_val p.polar)

      | _ -> failwith ("Runtime error: unexpected command at line "^(string_of_int i))
  in
  
  aux 0;;

(* To do:
  - Slow down execution
    An empty for loop executes 798 rounds in 1s,
    for specific functions (mainly display), measurements are needed
  - How do we differentiate
    
    "ABC"EOL
    ?->X

    from
    
    "ABC"?->X
    
    ?
    Both are compiled as [| String ["A"; "B"; "C"]; Assign (QMark, Var 23) |] for the moment. *)