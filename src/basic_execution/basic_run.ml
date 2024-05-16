(* Execution of Basic code *)


(* General execution function *)
let run_program (win : Sdlwindow.t) (ren : Sdlrender.t)
  (p : parameters) (proj : project_content) ((code, proglist): basic_code)
  (entry_index : int) (verbose : bool) : unit =
  
  (** Initialization of all parameters **)

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
  (* string_seen: to decide whether to print "Done" or not at the end of the execution *)
  let string_seen = ref false in

  (* text_screen = true if the text screen is currently displayed, false if it is the graphic screen *)
  let text_screen = ref true in
  
  (* Time of last color switch, to prevent flashing effect *)
  let last_switch = ref (Unix.gettimeofday ()) in

  (* Initialization of the parameters *)
  
  exit_key_check := false;
  parameters_updated := true;
  escape_activated := true;
   
  clear_text ();
  writing_index := -1;
  background_changed := false;
  
  
  (* Ends the execution of the program *)
  let end_execution () =
    quit_print win ren !val_seen !last_val p.polar !string_seen !text_screen
  in  

  (** Main looping function **)
  let rec aux (i : int) : unit =
    
    if verbose then
      (print_string "Index: ";
      print_int i;
      print_newline ());

    (* Debug, display of variable content *)
    (* (try
      (* print_endline ("Y = "^(string_of_float (access_real_var p.var 24))); *)
      (* let z = eval_num p (let e,_ = extract_expr ["DIM"; "LIST"; "ANS"] in e) in
      print_string ("Dim List Ans = "^(string_of_float (z.re)));
      print_newline (); *)
      ()
    with
      | _ -> ()); *)

    (* Pause for 1/798s, overridden by Press on Tab *)
    if slowdown_condition () then
      Unix.sleepf timer.general;

    (* Switch to or from dark mode *)
    if !key_pressed = Sdlkeycode.RCtrl then
      (let time = Unix.gettimeofday () in
      if time -. !last_switch > 0.5 then
        (last_switch := time;
        switch_color_mode ();
        parameters_updated := true));

    (* End of the execution *)
    if i >= n then
      end_execution ()
    else if !exit_key_check then
      raise Window_Closed
    else

    let _ =
      if !parameters_updated then
        if !text_screen
          then tdraw ren
          else gdraw ren
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

      | Assign (QMark, v) -> (* to do: treat list/mat assignment *)
        (text_screen := true;
        let e = qmark win ren in
        match eval_entity p e with
          | Value z ->
            assign_var p (Value z) v;
            aux (i+1)
          (* TO DO: list_expr, mat_expr *)
          | _ -> aux (i+1))

      | Assign (e, v) ->
        let z = eval_num p e in
        (last_val := z;
        val_seen := true;
        (match v with
          | Var vi ->
            ((if vi <= 28
              || z.im = 0.
                &&
                (z.re >= 0.
                || not (List.mem vi [53; 55; 58; 59; 65; 66; 69]))
              then assign_var p (Value z) v
            else run_fail i ("Incorrect value for variable of index "^(string_of_int vi)));
            (* Specific actions for window variables *)
            if vi >= xmin_index && vi <= yscl_index then
              (let xmin = access_real_var p.var xmin_index in
              let xmax = access_real_var p.var xmax_index in
              (* Resetting Xdot *)
              set_real_var p.var xdot_index ((xmax -. xmin) /. 126.);
              wipe gscreen;
              background_changed := true)
            )

          | ListIndex (Arithm [Entity a], iexp) ->
            (let vala = get_val_numexpr p a in
            let ie = eval_num p iexp in
            if not (is_int vala && is_int ie)
              then run_fail i "Wrong index for list";
            assign_var p (Value z) (ListIndex (Complex vala, Complex ie)))

          | ListIndex (StringExpr (Str_content sl), iexp) ->
            (try
              let ai = list_index_from_string p.listfile p.listzero sl in
              let ie = eval_num p iexp in
              if not (is_int ie)
                then run_fail i "Wrong index for list";
              assign_var p (Value z) (ListIndex (Complex (complex_of_int (ai+1)), Complex ie))
            with
              | Not_found -> run_fail i "List string index not found in assignment to list element")

          | MatIndex (ai, iexp, jexp) ->
            (let ie = eval_num p iexp in
            let je = eval_num p jexp in
            if not (is_int ie && is_int je)
              then run_fail i "Wrong index for matrix";
            (* let m = p.mat.(int_of_complex vala) in
            let iint = int_of_complex ie in
            let jint = int_of_complex je in
            m.(iint).(jint) <- z.re;
            m.(iint + (Array.length m)/2).(jint) <- z.im) *)
            assign_var p (Value z)
              (MatIndex (ai,
                Complex ie, Complex je)))

          | _ -> run_fail i "Assignment to unassignable object"
        );
        if i<n-1 && code.(i+1) = Disp then
          (text_screen := true;
          line_feed ();
          print_number z p.polar;
          disp p ren writing_index;
          aux (i+2))
        else aux (i+1))

      | Graphic g ->
        (apply_graphic ren p i g text_screen verbose;
        aux (i+1))
      
      | Expr (Complex z) ->
        (store_ans p.var z;
        last_val := z;
        val_seen := true;
        if i<n-1 && code.(i+1) = Disp then
          (text_screen := true;
          line_feed ();
          print_number z p.polar;
          disp p ren writing_index;
          aux (i+2))
        else aux (i+1))

      (* Special case, to detect List _[0] *)
      | Expr (Arithm [Entity (Variable (ListIndex (ea , ej)))]) ->
        
        let j = eval_num p ej in
        if is_zero j then

          (* Handle it like a normal string *)
          (let a =
            match ea with
              | Complex z ->
                if is_int z then
                  int_of_complex z - 1
                else run_fail i "Wrong list index in \"List _ [_]\" expression"
              | Arithm _ ->
                let z = eval_num p ea in
                if is_int z then
                  int_of_complex z - 1
                else run_fail i "Wrong list index in \"List _ [_]\" expression"
              | StringExpr (Str_content sl) ->
                (try
                  list_index_from_string p.listfile p.listzero sl
                with
                  | Not_found -> failwith "List string index not found in \"List _ [_]\" expression")
              | _ -> failwith "Wrong list index in \"List _ [_]\" expression"
          in
          let s = p.listzero.(a) in
          let sl = rev_lexlist_to_rev_symblist s true verbose in
          text_screen := true;
          line_feed ();
          clear_line !writing_index;
          let len = List.length sl in
          (if len >= 21 then
            if len < 147 (* 21*7 *)
              then display_long_string sl
              else display_extra_long_string sl
          else
            locate_no_refresh sl 0 !writing_index);
          tdraw ren;
          val_seen := false;
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

        else
          (* Handle it like a numerical expression *)
          (let z = eval_num p (Arithm [Entity (Variable (ListIndex (ea , Complex j)))]) in
          store_ans p.var z;
          last_val := z;
          val_seen := true;
          if i<n-1 && code.(i+1) = Disp then
            (text_screen := true;
            line_feed ();
            print_number z p.polar;
            disp p ren writing_index;
            aux (i+2))
          else aux (i+1))

      
      | Expr (Arithm al) ->
        (match eval_entity p (Arithm al) with
          | Value z ->
            (store_ans p.var z;
            last_val := z;
            val_seen := true;
            if i<n-1 && code.(i+1) = Disp then
              (text_screen := true;
              line_feed ();
              print_number z p.polar;
              disp p ren writing_index;
              aux (i+2))
            else aux (i+1))

          (* Just storing in List Ans/Mat Ans *)
          (* Display is not treated yet *)
          | ListContent t ->
            (p.list.(6 * 26) <- numexpr_to_float_array t;
            if i<n-1 && code.(i+1) = Disp then
              aux (i+2) (* Display to be treated here *)
            else aux (i+1))

          | MatContent m ->
            (p.mat.(26) <- numexpr_to_float_matrix m;
            if i<n-1 && code.(i+1) = Disp then
              aux (i+2) (* Display to be treated here *)
            else aux (i+1))
          
          | _ -> run_fail i "Wrong output type of eval_entity")
      
      | String se ->
        (* A string alone is printed, an application of string function is not *)
        (string_seen := true;
        match se, eval_str p se with
          | Str_content _, Str_content s ->
            (let sl = rev_lexlist_to_rev_symblist s true verbose in
            text_screen := true;
            line_feed ();
            clear_line !writing_index;
            let len = List.length sl in
            (if len >= 21 then
              if len < 147 (* 21*7 *)
                then display_long_string sl
                else display_extra_long_string sl
            else
              locate_no_refresh sl 0 !writing_index);
            tdraw ren;
            val_seen := false;
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

          | _, Str_content sl ->
            (* Uninteresting case, prints a "Done" *)
            (* To be redone when we take care of the Dones *)
            if i<n-1 && (code.(i+1) = Disp
              || (code.(i+1) = End && !prog_goback = [])) then
              (text_screen := true;
              line_feed ();
              clear_line !writing_index;
              locate_no_refresh ["e"; "n"; "o"; "D"] 17 !writing_index;
              tdraw ren;
              if code.(i+1) = Disp then
                (disp p ren writing_index;
                aux (i+1))
              else quit win ren true (* Quit after the string *))
          | _, Num_expr _ -> run_fail i "String expression has numerical value"
          | _  -> run_fail i "Wrong type in string expression evaluation")
          
      | Locate (e1, e2, se) ->
        let z1 = eval_num p e1 in
        let z2 = eval_num p e2 in
        (if not
          ((is_int z1) && (is_int z2)
          && (z1.re >= 1.) && (z1.re <= 21.)
          && (z2.re >= 1.) && (z2.re <= 7.))
          then run_fail i "Wrong arguments for Locate";
        (* The coordinates in Casio Basic are between 1 and 21 *)
        text_screen := true;
        let sl =
          match eval_str p se with
            | Str_content s -> rev_lexlist_to_rev_symblist s true verbose
            | Num_expr (Complex z) ->
              (if z.im <> 0. (* In Casio Basic, Locate does not handle complex numbers *)
                then run_fail i "A number printed by Locate cannot be a complex number";
              let z_repr = float_to_casio z.re in
              str_to_rev_symblist_simple z_repr)
            | _ -> run_fail i "Wrong output type for string expression evaluation"
        in
        locate ren sl ((int_of_complex z1)-1) ((int_of_complex z2)-1);
        val_seen := false;
        string_seen := true;
        if slowdown_condition () then
          Unix.sleepf timer.locate;
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

      | Function ("CLRTEXT", _) ->
        (clear_text ();
        text_screen := true;
        tdraw ren;
        writing_index := -1;
        val_seen := false;
        string_seen := false;
        aux (i+1))
      
      | For (vi, e1, e2, e3, inext) ->
        (let z1 = eval_num p e1 in
        if z1.im <> 0.
          then run_fail i "Complex value given to a For loop";
        set_var p.var vi z1;

        let z2 = eval_num p e2 in
        let z3 = eval_num p e3 in
        if z2.im <> 0. || z3.im <> 0.
          then run_fail i "Complex value given to a For loop"
        else if is_zero z3
          then run_fail i "The step value of a For loop is 0";
          
        let asc = z3.re > 0. in
        for_info := (vi, i+1, asc, z2.re, z3.re)::!for_info;

        (* First test of the loop condition *)
        let xi = access_real_var p.var vi in
        if (asc && xi <= z2.re) || ((not asc) && xi >= z2.re)
          then aux (i+1)
          else
            (for_info := List.tl !for_info;
            aux inext) (* The loop is finished before starting *)
        )
      
      | Next ->
        (match !for_info with
          | (vari, iback, asc, vto, vstep)::_ ->
            (let xi = (access_real_var p.var vari) +. vstep in
            if (asc && xi <= vto) || ((not asc) && xi >= vto)
              then
                (set_real_var p.var vari xi;
                aux iback)
              else (* Exitting the loop *)
                (for_info := List.tl !for_info;
                aux (i+1)))
          | [] -> run_fail i "Unexpected Next")

      | Breakfor j ->
        (match !for_info with
          | [] -> run_fail i "Unexpected Break"
          | _::t ->
            (for_info := t;
            aux j)
        )
      
      | Prog name ->
        (prog_goback := (i+1)::!prog_goback;
        let j =
          try
            List.assoc name proglist
          with
            | Not_found -> run_fail i ("Prog \""^name^"\" not found")
        in
        aux j)
      
      | AssignStr (se, si) ->
        let sl =
          match eval_str p se with
            | Str_content s -> s
            | _ -> run_fail i ("Wrong input type in string assignment")
        in
        ((match si with
          | Str_access j -> p.str.(j) <- sl
          | ListIndexZero (Arithm [Entity a], e) ->
            if is_zero (eval_num p e) then
              (let vala = get_val_numexpr p a in
              let ai = int_of_complex vala in
              p.listzero.(26 * p.listfile + ai - 1) <- sl;
              if Array.length p.list.(26 * p.listfile + ai - 1) = 0 then
                p.list.(26 * p.listfile + ai - 1) <- [|0.;0.|])
            else
              run_fail i "List index should be 0 in string assignment"
          | ListIndexZero (StringExpr (Str_content sl), e) ->
            if is_zero (eval_num p e) then
              (let ai =
                try
                  list_index_from_string p.listfile p.listzero sl
                with
                | Not_found ->
                  (* Look for the first empty list *)
                  index_of_first_empty_list p.listfile p.list
              in
              p.listzero.(26 * p.listfile + ai) <- sl;
              if Array.length p.list.(6 * p.listfile + ai) = 0 then
                p.list.(26 * p.listfile + ai) <- [|0.;0.|])
              else
                run_fail i "List index should be 0 in string assignment"
          | _ -> run_fail i "Wrong string in string assignment");
        aux (i+1))

      | AssignList (le, ei) ->
        (* Code on top of older code, can be simplified... *)
        let t = eval_list p le in
        let ni =
          match ei with
            | Arithm [Entity n] -> get_val_numexpr p n
            | StringExpr (Str_content sl) ->
              (try
                let ai = list_index_from_string p.listfile p.listzero sl in
                complex_of_int (1 + ai)
              with
                | Not_found ->
                  (* Look for the first empty list *)
                  let a_empty = index_of_first_empty_list p.listfile p.list in
                  complex_of_int (1 + a_empty)
              )
            | _ -> run_fail i "Wrong parameter in list assignment"
        in
        let nii =
          match ei with
            | Arithm [Entity (Variable (Var 28))] (* Ans *) -> 27
            | _ ->
              if is_int ni
                then int_of_complex ni
                else run_fail i "Wrong index for list assignment"
        in
        ((if nii >= 1 && nii <= 26 then
          p.list.(26 * p.listfile + nii - 1) <- t
        else if nii = 27 then
          p.list.(6 * 26) <- t
        else
          run_fail i "Index out of bounds for list assignment");
        aux (i+1))
        
      | AssignMat (me, mi) ->
        let m = eval_mat p me in
        (p.mat.(if mi = 28 then 26 else mi) <- m;
        aux (i+1)) 

      | AssignMult (e, vi1, vi2) ->
        let z = eval_num p e in
        (last_val := z;
        val_seen := true;
        for vi = vi1 to vi2 do
          (* assign_var p (Value z) (Var vi) *)
          set_var p.var vi z
        done;
        aux (i+1))

      | Function ("FILL", [e; Arithm [Entity n]]) ->
        let z = eval_num p e in
        ((match n with
          | VarList (Arithm [Entity (nl)]) ->
            (let li = get_val_numexpr p nl in
            if is_int li then
              let lii = int_of_complex li in
              let len = (Array.length p.list.(26 * p.listfile + lii - 1)) / 2 in
              (for k = 0 to len-1 do
                p.list.(26 * p.listfile + lii - 1).(k) <- z.re;
                p.list.(26 * p.listfile + lii - 1).(k+len) <- z.im
              done)
            else
              run_fail i "Incorrect parameter in Fill")
          | VarList (StringExpr (Str_content sl)) ->
            (try
              let ai = list_index_from_string p.listfile p.listzero sl in
              let len = (Array.length p.list.(26 * p.listfile + ai)) / 2 in
              (for k = 0 to len-1 do
                p.list.(26 * p.listfile + ai).(k) <- z.re;
                p.list.(26 * p.listfile + ai).(k + len) <- z.im
              done)
            with
              | Not_found -> run_fail i "List string index not found in Fill parameter")
          | VarMat mi ->
            (let row = (Array.length p.mat.(mi)) / 2 in
            if row <> 0 then
              let col = Array.length p.mat.(mi).(0) in
              for a = 0 to row-1 do
                for b = 0 to col-1 do
                  p.mat.(mi).(a).(b) <- z.re;
                  p.mat.(mi).(a+row).(b) <- z.im
                done
              done)
          | _ -> run_fail i "Wrong arguments for command Fill");
        aux (i+1))

      | End ->
        (match !prog_goback with
          | j::t ->
            (prog_goback := t;
            aux j)
          | [] -> end_execution ())

      | Function ("STOP", _) ->
        (* Hard stop: ends the execution without returning to the calling program *)
        end_execution ()

      (* Disp that was not handled by a string *)
      | Disp ->
        ((if !text_screen
          then disp p ren writing_index
          else
            (disp_graphic ren (i<n-1 && (code.(i+1) <> End || !prog_goback <> []))));
        aux (i+1))

      | Function ("MENU", (StringExpr sexptitle) :: entry_list) ->
        (* Evaluation of the title *)
        (let title =
          match eval_str p sexptitle with
            | Str_content s -> skip_k ((List.length s)-19) (rev_lexlist_to_rev_symblist s true verbose)
            | _ -> run_fail i "Wrong Menu title"
        in
        let nentry = (List.length entry_list) / 2 in
        let entry_t = Array.make nentry ([], -1) in

        (* Computation of the entry array *)
        let rec entry_eval_loop k el =
          if k >= 0 then
            (match el with
              | StringExpr se :: Complex z :: t ->
                (let s =
                  match eval_str p se with
                    | Str_content s ->
                      skip_k ((List.length s)-16) (rev_lexlist_to_rev_symblist s true verbose)
                    | _ -> run_fail i "Wrong Menu string argument"
                in
                entry_t.(k) <- (s, int_of_complex z);
                entry_eval_loop (k-1) t)
              
              | _ -> run_fail i "Error in Menu arguments")
        in
        entry_eval_loop (nentry - 1) entry_list;

        (* Execution of the command *)
        let chosen_lbl = menu_command ren p title entry_t !text_screen in
        (if !text_screen
          then tdraw ren
          else gdraw ren);
        aux chosen_lbl)

      | Function ("FILE", [e]) ->
        let z = eval_num p e in
        if is_int z && z.re >= 1. && z.re <= 6. then
          (p.listfile <- int_of_complex z - 1;
          aux (i+1))
        else
          run_fail i "Wrong File argument"

      (* Errors *)
      | Function ("MENU", _) -> run_fail i "Wrong arguments for Menu"
      | _ -> run_fail i "Unexpected command"
  in

  (* let key_check_domain = Domain.spawn launch_key_check in *)

  (* (try
    let entry_index =
      try
        List.assoc entry_point proglist
      with
        | Not_found -> failwith ("No program named \""^entry_point^"\"")
    in
    aux entry_index;
    print_endline "--- End of the execution ---";
  with
    | Runtime_interruption
    | Window_Closed -> print_endline "--- Runtime interruption ---"
    | Invalid_argument s
    | Failure s -> print_endline s
    | Not_found -> print_endline "Runtime error: Not_found"); *)

  aux entry_index;;

  (* exit_key_check := true;
  Domain.join key_check_domain;
  close_graph win;
  Sdl.quit ();; *)