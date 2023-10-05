(* Execution of Basic code *)


(* General execution function *)
let run (proj : project_content) ((code, proglist): basic_code) (entry_point : string) : unit =
  
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

    (* debug *)
    print_endline (string_of_int i);

    (* Pause for 1/798s *)
    (* Overridden by Press on Tab *)
    if !key_pressed <> Tab then
    Unix.sleepf 0.001253133;

    (* End of the execution *)
    if i >= n then
      quit_print win ren !val_seen !last_val p.polar
    else if !exit_key_check then
      raise Window_Closed
    else

    let _ =
      if !parameters_updated then
        tdraw ren
    in

    (* Execution of the next command *)
    (* The commands are ordered in the match by order of frequency in usual programs *)
    match code.(i) with
      | Goto j -> aux j

      | If (e,j) ->
        let z = eval_num p e in
        if z.im = 0. && not (is_zero_float z.re)
          then aux (i+1)
          else aux j

      | JumpIf (e,j) ->
        if is_not_zero (eval_num p e)
          then aux j
          else aux (i+1)
      
      | Expr (Complex z, _) ->
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
      
      | Expr (Arithm al, expr_type) ->
        (match expr_type with
          | Numerical ->
            let z = eval_num p (Arithm al) in
            (store_ans p.var z;
            last_val := z;
            val_seen := true;
            if i<n-1 && code.(i+1) = Disp then
              (line_feed ();
              print_number z p.polar;
              disp p ren writing_index;
              aux (i+2))
            else aux (i+1))
          | ListExpr (* TO DO: list_expr, mat_expr *)
          | _ -> aux (i+1))
        
          
      | Assign (QMark, v) -> (* to do: treat list/mat assignment *)
        let (e, expr_type) = qmark win ren in
        (match expr_type with
          | Numerical ->
            let z = eval_num p e in
            assign_var p (Value z) v;
            aux (i+1)
          | ListExpr (* TO DO: list_expr, mat_expr *)
          | _ -> aux (i+1))

      | Assign (e, v) ->
        let z = eval_num p e in
        (last_val := z;
        val_seen := true;
        (match v with
          | Var vi ->
            (* (p.var.(vi) <- z.re;
            p.var.(vi+29) <- z.im) *)
            assign_var p (Value z) v

          | ListIndex (a, iexp) ->
            (let vala = get_val_numexpr p a in
            let ie = eval_num p iexp in
            if not (is_int vala && is_int ie)
              then failwith "Runtime error: wrong index for list";
            (* let t = p.list.(int_of_complex vala) in
            let iint = int_of_complex ie in
            t.(iint) <- z.re;
            t.(iint + (Array.length t)/2) <- z.im) *)
            assign_var p (Value z) (ListIndex (Value vala, Complex ie)))

          | MatIndex (ai, iexp, jexp) ->
            (let ie = eval_num p iexp in
            let je = eval_num p jexp in
            if not (is_int ie && is_int je)
              then failwith "Runtime error: wrong index for matrix";
            (* let m = p.mat.(int_of_complex vala) in
            let iint = int_of_complex ie in
            let jint = int_of_complex je in
            m.(iint).(jint) <- z.re;
            m.(iint + (Array.length m)/2).(jint) <- z.im) *)
            assign_var p (Value z)
              (MatIndex (ai,
                Complex ie, Complex je)))

          | _ -> failwith "Runtime error: assignment to unassignable object"
        );
        if i<n-1 && code.(i+1) = Disp
          then
            (line_feed ();
            print_number z p.polar;
            disp p ren writing_index;
            aux (i+2))
          else aux (i+1))
      
      | String se ->
        (* A string alone is printed, an application of string function is not *)
        (match se, eval_str p se with
          | Str_content _, Str_content s ->
            (let sl = rev_lexlist_to_rev_symblist s true in
            line_feed ();
            clear_line !writing_index;
            let len = List.length sl in
            (if len >= 21 then
              display_long_string sl
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
              else if i<n-1 && code.(i+1) = Disp then
                (disp p ren writing_index;
                aux (i+2))
              else aux (i+1))

          | _, Str_content sl -> (* Uninteresting case, prints a "Done" *)
            (* To be redone when we take care of the Dones *)
            (line_feed ();
            clear_line !writing_index;
            locate_no_refresh ["e"; "n"; "o"; "D"] 0 !writing_index;
            tdraw ren;
            if (i <= n-2 && code.(i+1) = End
              || i <= n-3 && code.(i+1) = Disp && code.(i+2) = End)
              && !prog_goback = []
              then (* End of the program *)
                (if code.(i+1) = Disp
                  then disp p ren writing_index;
                quit win ren true (* Quit after the string *))
              else if i<n-1 && code.(i+1) = Disp then
                (disp p ren writing_index;
                aux (i+2))
              else aux (i+1))
          | _, Num_expr _ -> failwith "Runtime error: string expression has numerical value"
          | _  -> failwith "Runtime error: wrong type in string expression evaluation")
          
      | Locate (e1, e2, se) ->
        let z1 = eval_num p e1 in
        let z2 = eval_num p e2 in
        (if not
          ((is_int z1) && (is_int z2)
          && (z1.re >= 1.) && (z1.re <= 21.)
          && (z2.re >= 1.) && (z2.re <= 7.))
          then failwith "Runtime error: wrong arguments for Locate";
        (* The coordinates in Casio Basic are between 1 and 21 *)
        let sl =
          match eval_str p se with
            | Str_content s -> rev_lexlist_to_rev_symblist s true
            | Num_expr (Complex z) ->
              (if z.im <> 0. (* In Casio Basic, Locate does not handle complex numbers *)
                then failwith "Runtime error: a number printed by Locate cannot be a complex number";
              let z_repr = float_to_casio z.re in
              str_to_rev_symblist_simple z_repr)
            | _ -> failwith "Runtime error: wrong output type for string expression evaluation"
        in
        locate ren sl ((int_of_complex z1)-1) ((int_of_complex z2)-1);
        val_seen := true;
        if (i <= n-2 && code.(i+1) = End
          || i <= n-3 && code.(i+1) = Disp && code.(i+2) = End)
          && !prog_goback = [] then
            (* End of the program *)
            (if code.(i+1) = Disp
              then disp p ren writing_index;
            quit win ren true (* Quit after the string *))
          else if i<n-1 && code.(i+1) = Disp then
            (disp p ren writing_index;
            aux (i+2))
          else aux (i+1))

      | Function "CLRTEXT" ->
        (clear_text ();
        tdraw ren;
        writing_index := -1;
        val_seen := false;
        aux (i+1))
      
      | For (vi, e1, e2, e3, inext) ->
        (let z1 = eval_num p e1 in
        if z1.im <> 0.
          then failwith "Runtime error: complex value given to a For loop";
        p.var.(vi) <- z1.re;
        p.var.(vi+29) <- 0.;
        let z2 = eval_num p e2 in
        let z3 = eval_num p e3 in
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
        let j =
          try
            (* Debug *
            print_string "Prog looked for: ";
            print_endline name;
            List.iter (fun (s,k) -> (print_char '('; String.iter (fun c -> print_int (Char.code c); print_char ' ') s; print_string ", index = "; print_int k; print_string ") ")) proglist;
            print_newline ();
            print_string "name = ";
            String.iter (fun c -> print_int (Char.code c); print_char ' ') name;
            print_newline ();
            ****)
            List.assoc name proglist
          with
            | Not_found -> failwith ("Runtime error: Prog \""^name^"\" not found")
        in
        aux j)
      
      | AssignStr (se, si) ->
        let sl =
          match eval_str p se with
            | Str_content s -> s
            | _ -> failwith ("Runtime error: wrong type in string assignment")
        in
        (p.str.(si) <- sl;
        aux (i+1))

      | AssignList (le, n) ->
        let t = eval_list p le in
        let ni = get_val_numexpr p n in
        if is_int ni then
          (p.list.((int_of_complex ni)-1) <- t;
          aux (i+1))
        else failwith "Runtime error: wrong index for list assignment"

      | AssignMat (me, mi) ->
        let m = eval_mat p me in
        (p.mat.(mi) <- m;
        aux (i+1)) 

      | AssignMult (e, vi1, vi2) ->
        let z = eval_num p e in
        (last_val := z;
        val_seen := true;
        for vi = vi1 to vi2 do
          assign_var p (Value z) (Var vi)
        done;
        aux (i+1))

      | End ->
        (match !prog_goback with
          | j::t ->
            (prog_goback := t;
            aux j)
          | [] -> quit_print win ren !val_seen !last_val p.polar)

      | Function "STOP" ->
        (* Hard stop: ends the execution without returning to the calling program *)
        quit_print win ren !val_seen !last_val p.polar

      (* Ignored commands *)
      | Disp -> aux (i+1)

      | _ -> failwith ("Runtime error: unexpected command at line "^(string_of_int i))
  in
  
  (try
    let key_check_domain = Domain.spawn launch_key_check in
    let entry_index =
      try
        List.assoc entry_point proglist
      with
        | Not_found -> failwith ("No program named \""^entry_point^"\"")
    in
    aux entry_index;
    print_endline "--- End of the execution ---";
    exit_key_check := true;
    Domain.join key_check_domain
  with
    | Runtime_interruption
    | Window_Closed -> print_endline "--- Runtime interruption ---"
    | Failure s -> print_endline s
    | Not_found -> print_endline "Runtime error: Not_found");
  exit_key_check := true;
  close_graph win;
  Sdl.quit ();;

(* To do:
  - Slow down execution
    An empty for loop executes 798 rounds in 1s,
    for specific functions (mainly display), measurements are needed *)