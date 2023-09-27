(* Execution of Basic code *)


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
  exit_key_check := false;
  parameters_updated := true;
  escape_activated := true;
  getkey := 0;
  key_pressed := Unknown;

  (** Main looping function **)
  let rec aux (i : int) : unit =

    (* End of the execution *)
    if i >= n then
      quit_print win ren !val_seen !last_val p.polar
    else if !exit_key_check then
      raise Window_Closed
    else

    let _ =
      if !parameters_updated
        then
          tdraw ren
    in

    (* Execution of the next command *)
    match code.(i) with
      | Goto j -> aux j

      | If (e,j) ->
        let z = eval p e in
        if z.im = 0. && not (is_zero_float z.re)
          then aux (i+1)
          else aux j

      | JumpIf (e,j) ->
        if is_not_zero (eval p e)
          then aux j
          else aux (i+1)
          
      | Expr (Arithm al, _) -> (* To do: treat list_expr, mat_expr *)
        let z = eval p (Arithm al) in
        (store_ans p.var z;
        last_val := z;
        val_seen := true;
        if i<n-1 && code.(i+1) = Disp
          then
            (line_feed ();
            print_number z p.polar;
            disp p ren writing_index;
            aux (i+2))
          else aux (i+1))
          
      | Assign (QMark, v) ->
        (let e = qmark win ren in
        let z = eval p e in
        assign_var p (Value z) v;
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
            assign_var p (Value z) (ListIndex (Value vala, Arithm [Entity (Value ie)])))

          | MatIndex (ai, iexp, jexp) ->
            (let ie = eval p iexp in
            let je = eval p jexp in
            if not (is_int ie && is_int je)
              then failwith "Runtime error: wrong index for matrix";
            (* let m = p.mat.(int_of_complex vala) in
            let iint = int_of_complex ie in
            let jint = int_of_complex je in
            m.(iint).(jint) <- z.re;
            m.(iint + (Array.length m)/2).(jint) <- z.im) *)
            assign_var p (Value z)
              (MatIndex (ai,
                Arithm [Entity (Value ie)], Arithm [Entity (Value je)])))

          | _ -> failwith "Runtime error: assignment to unassignable object"
        );
        if i<n-1 && code.(i+1) = Disp
          then
            (line_feed ();
            print_number z p.polar;
            disp p ren writing_index;
            aux (i+2))
          else aux (i+1))
      
      | String sl ->
        (line_feed ();
        clear_line !writing_index;
        let len = List.length sl in
        (if len > 21 then
          (let (slk,slnk) = split_k sl (len-21) in
          locate_no_refresh (List.rev slnk) 0 !writing_index;
          line_feed ();
          locate_no_refresh (List.rev slk) 0 !writing_index)
        else
          locate_no_refresh sl 0 !writing_index);
        tdraw ren;
        val_seen := true;
        if (i <= n-2 && code.(i+1) = End
          || i <= n-3 && code.(i+1) = Disp && code.(i+2) = End)
          && !prog_goback = []
          then (* End of the program *)
            (if code.(i+1) = Disp
              then disp p ren writing_index;
            quit win ren true (* Quit after the string *))
          else if i<n-1 && code.(i+1) = Disp
            then
              (disp p ren writing_index;
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
        locate ren sl ((int_of_complex z1)-1) ((int_of_complex z2)-1);
        val_seen := true;
        if (i <= n-2 && code.(i+1) = End
          || i <= n-3 && code.(i+1) = Disp && code.(i+2) = End)
          && !prog_goback = []
          then (* End of the program *)
            (if code.(i+1) = Disp
              then disp p ren writing_index;
            quit win ren true (* Quit after the string *))
          else if i<n-1 && code.(i+1) = Disp
          then
            (disp p ren writing_index;
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
    (* To do: let the user specify an entry point:
    Add (entry_point : string) as a parameter of run
    
    let entry_index = List.assoc entry_point proglist in
    aux entry_index
    *)
    let key_check_domain = Domain.spawn launch_key_check in
    aux 0;
    exit_key_check := true;
    Domain.join key_check_domain
  with
    | Runtime_interruption
    | Window_Closed -> print_endline "--- Runtime interruption ---"
    | Failure s -> print_endline s);
  exit_key_check := true;
  close_graph win;
  Sdl.quit ();;

(* To do:
  - Slow down execution
    An empty for loop executes 798 rounds in 1s,
    for specific functions (mainly display), measurements are needed *)