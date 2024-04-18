(* Compilation of Basic code *)

(* Compiles the list of lexemes lexlist by modifying the array code in place. *)
(* Returns the proglist parameter of the working memory *)
let process_commands (code : (command array) ref) (prog : ((string * (string list)) list))
  (verbose : bool) (display_all : bool) (ignore_errors : bool) : (string * int) list =
  let mem =
    {
      stack = [];
      lblindex = Array.make 38 (-1);
      gotoindex = [];
      progindex = [];
      menustack = [];
    }
  in

  (* Looping function *)
  
  let rec aux (lexlist : string list) (i : int) : int =

    (* Debug *)
    if display_all then
      (try
        let (line, _) = extract_line lexlist in
        print_endline ("i = "^(string_of_int i)^" -> "^(String.concat " " (List.rev (List.map String.escaped line))))
      with
        | _ -> ());

    (* Expression handling *)
    let (e, t) =
      try
        extract_expr lexlist
      with
        | Failure s -> fail lexlist i s
    in
    let (expr_found, j, next_t) =
      (match e, t with
      
        | Arithm [], _ ->
          (* The beginning of lexlist is not an expression *)
          (false, -1, [])

        | _, "ASSIGN"::t' ->
          (match t' with
            | [v] ->
              (if is_var v then
                (set code i (Assign (e, Var (var_index v)));
                (true, i+1, []))
              else fail lexlist i "Compilation: Wrong assignment (->) of a variable")
            | v::"EOL"::t''
            | v::"COLON"::t''
            | v::"DISP"::t'' ->
              if is_var v then
                (set code i (Assign (e, Var (var_index v)));
                (true, i+1, List.tl t'))
              else
                fail lexlist i "Compilation error: Wrong assignment (->) of a variable"
            (* e -> X~Z *)
            | v1::"\126"::v2::t'' ->
              if is_letter_var v1 && is_letter_var v2 then
                let vi1 = var_index v1 in
                let vi2 = var_index v2 in
                if vi2 >= vi1 then
                  (set code i (AssignMult (e, vi1, vi2));
                  (true, i+1, t''))
                else fail lexlist i "Compilation error: Wrong order in multiple assignment (~)"
              else fail lexlist i "Compilation error: Wrong multi-assignment (-> ~) of a variable"
            
            | "LIST"::_::"LSQBRACKET"::_
            (* All digits are specified, to avoid "List 1 EOL [[1,2][3,4]] -> Mat A" being parsed as "List _ _ [" *)
            | "LIST"::_::"0"::"LSQBRACKET"::_
            | "LIST"::_::"1"::"LSQBRACKET"::_
            | "LIST"::_::"2"::"LSQBRACKET"::_
            | "LIST"::_::"3"::"LSQBRACKET"::_
            | "LIST"::_::"4"::"LSQBRACKET"::_
            | "LIST"::_::"5"::"LSQBRACKET"::_
            | "LIST"::_::"6"::"LSQBRACKET"::_
            | "LIST"::_::"7"::"LSQBRACKET"::_
            | "LIST"::_::"8"::"LSQBRACKET"::_
            | "LIST"::_::"9"::"LSQBRACKET"::_ ->
              let (li,t'') = extract_list_index (List.tl t') in
              (match li with
                | Entity (Variable lx) ->
                  (set code i (Assign (e, lx));
                  (true, i+1, t''))
                | _ -> fail lexlist i "Compilation error: Wrong assignment (->) of a list element")

            | "MAT"::_::"LSQBRACKET"::_ ->
              let (mi,t'') = extract_mat_index (List.tl t') in
              (match mi with
                | Entity (Variable mx) ->
                  (set code i (Assign (e, mx));
                  (true, i+1, t''))
                | _ -> fail lexlist i "Compilation error: Wrong assignment (->) of a matrix element")

            | "LIST" :: "QUOTE" :: q ->
              let (sl, q') = aux_extract_str q in
              (set code i (AssignList (e, StringExpr (Str_content sl)));
              (true, i+1, q'))

            | "LIST"::a::t'' ->
              (* Numerical is allowed, because entity functions have unspecified output type *)
              if is_letter_var a || a = "ANS" then
                (set code i (AssignList (e, Arithm [Entity (Variable (Var (var_index a)))]));
                (true, i+1, t''))
              else if is_digit a then
                let (vi,q) = read_int (a::t'') true in
                (set code i (AssignList (e, Arithm [Entity (Value (complex_of_int vi))]));
                (true, i+1, q))
              else fail lexlist i "Compilation error: wrong list index"

            | "MAT"::a::t'' ->
              if is_letter_var a && String.length a = 1 || a = "ANS" then
                (set code i (AssignMat (e, var_index a));
                (true, i+1, t''))
              else fail lexlist i "extract_mat_index: wrong matrix index"
            
            (* To do *)
            (* | "DIM"::"LIST"::"QUOTE"::q ->  *)

            | "DIM"::"LIST"::a::t'' ->
              let dim_expr = Arithm [Function ("Init", [e])] in
              if is_letter_var a || a = "ANS" then
                (set code i (AssignList (dim_expr, Arithm [Entity (Variable (Var (var_index a)))]));
                (true, i+1, t''))
              else if is_digit a then
                let (vi,q) = read_int (a::t'') true in
                (set code i (AssignList (dim_expr, Arithm [Entity (Value (complex_of_int vi))]));
                (true, i+1, q))
              else fail lexlist i "Compilation error: wrong list index in dimension assignment"

            

            | "DIM"::"MAT"::a::t'' ->
              let dim_expr = Arithm [Function ("Init", [e])] in
              if is_letter_var a && String.length a = 1 || a = "ANS" then
                (set code i (AssignMat (dim_expr, var_index a));
                (true, i+1, t''))
              else fail lexlist i "Compilation error: wrong matrix index in dimension assignment"

            | _ -> fail lexlist i "Compilation error: Syntax error during assignment"
          )
        
        (* QMark is treated in the case above *)
        | QMark, _ -> fail lexlist i "Compilation error: Syntax error, ? should be followed by ->"
          
        (* e => command *)
        (* Compiled as
          i          If (e, j)
          i+1..j-1   command
          j          ...
        *)
        | _, "IMPL"::t' ->
          let (next_line, t'') = extract_line t' in
          (* Compilation of only the next line *)
          (try
            let j = aux (List.rev next_line) (i+1) in
            set code i (If (e, j-1));
            (true, j-1, t'')
          with
            | Compilation_error (_, i, error_message) -> fail lexlist i error_message)
        
        | _, "EOL"::_
        | _, "COLON"::_
        | _, "DISP"::_
        | _, [] ->
          (* Simple expression *)
          (set code i (Expr e); 
          (true, i+1, t))

        | _ -> fail lexlist i "Compilation error: Syntax error after an expression"
      )
    in
    if expr_found then aux next_t j
    else

    (* String expression handling *)
    let (se, t) = extract_string_expr lexlist in
    let (str_expr_found, j, next_t) =
      match se, t with
        | Num_expr (Arithm []), _ -> (false, -1, [])
        | _, "EOL"::_
        | _, "DISP"::_
        | _, "COLON"::_
        | _, [] ->
          (set code i (String se);
          (true, i+1, t))
        | _, "ASSIGN"::"STR"::t' ->
          let (vi,t'') = read_int t' true in
          (set code i (AssignStr (se, Str_access (vi-1)));
          (true, i+1, t''))
        (* Assignment of a string expression to List _[0]
          ("0" stated explicitly, an expression of value 0 is not accepted by calculators) *)
        | _, "ASSIGN" :: "LIST" :: t' ->
          let (li,t'') = extract_list_index t' in
          (match li with
            | Entity (Variable (ListIndex (a, e))) ->
              (set code i (AssignStr (se, ListIndexZero (a, e)));
              (true, i+1, t''))
            | _ -> fail lexlist i "Compilation error: Wrong assignment (->) of a string to index 0 of a list")
        | _, "QMARK"::"ASSIGN"::_ -> (* "ABC"?->X *)
          (set code i (String se);
          (true, i+1, t))
        | _, "ASSIGN"::"FN"::t' ->
          (* Fn functionalities are not implemented:
             the line is ignored *)
          let (_, t'') = extract_line t in
          (true, i, t'')
        | _ -> fail t i "Compilation error: Syntax error after a string"
    in
    if str_expr_found then aux next_t j
    else
    
    (* Other lexemes *)
    match lexlist with
      
      | "EOL" :: t
      | "COLON" :: t -> aux t i
      
      | "IF" :: t ->
        let (j,t') = process_if i t code mem in
        aux t' j

      | "THEN" :: t ->
        let (j,t') = process_then i t code mem in
        aux t' j
          
      | "ELSE" :: t ->
        let (j,t') = process_else i t code mem in
        aux t' j

      | "IFEND" :: t ->
        let (j,t') = process_ifend i t code mem in
        aux t' j
      
      | "WHILE" :: t ->
        let (j,t') = process_while i t code mem in
        aux t' j
      
      | "WHILEEND" :: t ->
        let (j,t') = process_whileend i t code mem in
        aux t' j

      | "FOR" :: t ->
        let (j,t') = process_for i t code mem in
        aux t' j

      | "NEXT" :: t ->
        let (j,t') = process_next i t code mem in
        aux t' j

      | "DO" :: t ->
        (mem.stack <- ("do", i)::mem.stack;
        aux t i)

      | "LPWHILE" :: t ->
        let (j,t') = process_lpwhile i t code mem in
        aux t' j

      | "LOCATE" :: t ->
        let (j,t') = process_locate i t code mem in
        aux t' j

      | "DISP" :: t ->
        (set code i Disp;
        aux t (i+1))
      
      | "LBL" :: t ->
        let (j,t') = process_lbl i t code mem in
        aux t' j
      
      | "GOTO" :: t ->
        let (j,t') = process_goto i t code mem in
        aux t' j
      
      | "PROG" :: "QUOTE" :: t ->
        let (j,t') = process_prog i t code mem in
        aux t' j
      
      (* Comment ('): line ignored *)
      | "\039"::t ->
        let (_, t') = extract_line t in
        aux t' i

      | "RETURN" :: t ->
        (fail_if_not_eol lexlist i;
        set code i End;
        aux t (i+1))

      | "BREAK" :: t ->
        (fail_if_not_eol lexlist i;
        mem.stack <- ("break", i)::mem.stack;
        (* One spot is left empty for a Goto command to be inserted
          when the closing statement of the loop is encountered *)
        aux t (i+1))

      (* Commands with 0 arguments *)
      | "CLRTEXT" :: t
      | "STOP" :: t ->
        (fail_if_not_eol lexlist i;
        set code i (Function (List.hd lexlist, []));
        aux t (i+1))

      (* Commands with arguments *)

      | "ISZ" :: t
      | "DSZ" :: t ->
        (* Compiled as V + 1 -> V or V - 1 -> V *)
        (fail_if_not_eol t i;
        let isz = List.hd lexlist = "ISZ" in
        match t with
          | v :: t' ->
            if is_var v then
              (let vi = var_index v in
              let e =
                Arithm
                  [Entity (Variable (Var vi));
                  Op (if isz then "PLUS" else "MINUS");
                  Entity (Value {Complex.re = 1.; im = 0.})]
              in
              set code i (Assign (e, Var vi));
              aux t' (i+1))
            else fail t i ("Compilation error: Wrong variable for "^(if isz then "Isz" else "Dsz"))
          | _ -> fail t i ("Compilation error: "^(if isz then "Isz" else "Dsz")^" without specified variable"))
        
      | "CLRLIST" :: a :: t'' ->
        let empty_list = Arithm [Entity (ListContent [||])] in
        if is_letter_var a || a = "ANS" then (* ClrList A *)
          (set code i (AssignList (empty_list, Arithm [Entity (Variable (Var (var_index a)))]));
          aux t'' (i+1))
        else if is_digit a then (* ClrList 11 *)
          let (vi,q) = read_int (a::t'') true in
          (set code i (AssignList (empty_list, Arithm [Entity (Value (complex_of_int vi))]));
          aux q (i+1))
        else if a = "EOL" || a = "COLON" || a = "DISP" then (* ClrList (clears all lists)*)
          (for j = 1 to 20 do
            set code (i+j-1) (AssignList (empty_list, Arithm [Entity (Value (complex_of_int j))]))
          done;
          aux t'' (i+20))
        else fail lexlist i "Compilation error: Wrong index for ClrList"
      
      | ["CLRLIST"] ->
        (let empty_list = Arithm [Entity (ListContent [||])] in
        for j = 1 to 20 do
          set code (i+j-1) (AssignList (empty_list, Arithm [Entity (Value (complex_of_int j))]))
        done;
        aux [] (i+20))

      | "CLRMAT" :: a :: t'' ->
        let empty_mat = Arithm [Entity (MatContent [||])] in
        if is_letter_var a && String.length a = 1 || a = "ANS" then
          (set code i (AssignMat (empty_mat, var_index a));
          aux t'' (i+1))
        else if a = "EOL" || a = "COLON" || a = "DISP" then (* ClrMat (clears all matrices)*)
          (for j = 0 to 19 do
            set code (i+j) (AssignMat (empty_mat, j))
          done;
          aux t'' (i+20))
        else fail lexlist i "Compilation error: Wrong index for ClrMat"

      | ["CLRMAT"] ->
        (let empty_mat = Arithm [Entity (MatContent [||])] in
        for j = 0 to 19 do
          set code (i+j) (AssignMat (empty_mat, j))
        done;
        aux [] (i+20))

      | "FILL" :: t ->
        (match extract_expr t with
          | e, ","::"LIST"::"QUOTE"::q ->
            let (sl, q') = aux_extract_str q in
            (set code i (Function ("FILL", [e; Arithm [Entity (VarList (StringExpr (Str_content sl)))]]));
            aux (match q' with [] -> [] | "RPAR"::q'' -> q'' | _ -> q') (i+1))
          | e, ","::"LIST"::a::t' ->
            if is_letter_var a then
              (set code i (Function ("FILL", [e; Arithm [Entity (VarList (Arithm [Entity (Variable (Var (var_index a)))]))]]));
              aux (match t' with [] -> [] | "RPAR"::q -> q | _ -> t') (i+1))
            else if is_digit a then
              let (vi,q) = read_int (a::t') true in
              (set code i (Function ("FILL", [e; Arithm [Entity (VarList (Arithm [Entity (Value (complex_of_int vi))]))]]));
              aux (match q with [] -> [] | "RPAR"::q' -> q' | _ -> q) (i+1))
            else fail lexlist i "Compilation error: wrong list index in Fill command"

          | e, ","::"MAT"::a::t' ->
            if is_letter_var a && String.length a = 1 then
              (set code i (Function ("FILL", [e; Arithm [Entity (VarMat (var_index a))]]));
              aux (match t' with [] -> [] | "RPAR"::q -> q | _ -> t') (i+1))
            else fail lexlist i "extract_mat_index: wrong matrix index"

          | _ -> fail t i "Compilation error: Syntax error in Fill command")
        

      (* Graphic commands *)

      | "PLOTON" :: t
      | "PLOTOFF" :: t
      | "PLOTCHG" :: t ->
        let lex = List.hd lexlist in
        let suffix = String.sub lex 4 (String.length lex - 4) in
        let (el, t') = extract_list_content t in
        (match el with
          | [ex; ey] ->
            let command =
              match suffix with
                | "ON" -> PlotOn (ex, ey)
                | "OFF" -> PlotOff (ex, ey) 
                | "CHG" -> Graphic_Function ("PLOTCHG", [ex; ey])
                | _ -> fail lexlist i "Compilation error"
            in
            (set code i
              (Graphic command);
            aux t' (i+1))
          | _ ->
            fail t i
              ("Compilation error: Plot"^(String.capitalize_ascii suffix)^" expects two parameters"))
      
      | "FLINE" :: t
      | "CIRCLE" :: t
      | "HORIZONTAL" :: t
      | "VERTICAL" :: t
      | "SKETCHNORMAL" :: _ :: t
      | "SKETCHTHICK" :: _ :: t
      | "SKETCHBROKEN" :: _ :: t
      | "SKETCHDOT" :: _ :: t ->
        let (el, t') = extract_list_content t in
        let style, style_code =
          (match List.hd lexlist with
            | "SKETCHNORMAL" -> (Some StyleNormal), Complex (complex_of_float 1.)
            | "SKETCHTHICK" -> (Some StyleThick), Complex (complex_of_float 2.)
            | "SKETCHBROKEN" -> (Some StyleBroken), Complex (complex_of_float 3.)
            | "SKETCHDOT" -> (Some StyleDot), Complex (complex_of_float 4.)
            | _ -> None, Complex (complex_of_float 0.))
        in
        let line_type =
          (* 0 = F-line, 1 = Vertical, 2 = Horizontal, 3 = Circle *)
          match lexlist with
            | "FLINE" :: _
            | _ :: "FLINE" :: _ -> 0
            | "VERTICAL" :: _
            | _ :: "VERTICAL" :: _ -> 1
            | "HORIZONTAL" :: _
            | _ :: "HORIZONTAL" :: _ -> 2
            | _ -> 3
        in
        (match line_type, el with
          | 0, [ex1; ey1; ex2; ey2] ->
            (set code i (Graphic (Fline (ex1, ey1, ex2, ey2, style)));
            aux t' (i+1))
          | 1, [e]
          | 2, [e] ->
            (set code i (Graphic (Graphic_Function ((if line_type = 1 then "VERTICAL" else "HORIZONTAL"), style_code :: el)));
            aux t' (i+1))
          | 3, [ex; ey; er] ->
            (set code i (Graphic (Graphic_Function ("CIRCLE", style_code :: el)));
            aux t' (i+1))
          | _ ->
            fail t i ("Compilation error: "^
            (match line_type with
              | 0 -> "Fline expects four parameters"
              | 1 -> "Vertical expects one parameter"
              | 2 -> "Horizonal expects one parameter"
              | _ -> "Circle expects three parameters"))
        )

      | "PXLON" :: t
      | "PXLOFF" :: t
      | "PXLCHG" :: t ->
        let lex = List.hd lexlist in
        let suffix = String.sub lex 3 (String.length lex - 3) in
        let (el, t') = extract_list_content t in
        (match el with
          | [_; _] (* [ey; ex] *) ->
            (set code i
              (Graphic (Graphic_Function (lex, el)));
            aux t' (i+1))
          | _ ->
            fail t i
              ("Compilation error: Pxl"^(String.capitalize_ascii suffix)^" expects two parameters"))

      | "VIEWWINDOW" :: t ->
        let (el, t') = extract_list_content t in
        let len = List.length el in
        if len >= 1 && len <= 6 then
          (set code i (Graphic (ViewWindow el));
          aux t' (i+1))
        else
          fail t i "Compilation error: ViewWindow expects between 1 and 6 parameters"

      | "TEXT" :: t ->
        (match extract_list_content t with
          | [e1; e2; StringExpr se], q ->
            (set code i (Graphic (Text (e1, e2, se)));
            aux q (i+1))
          | [e1; e2; e3], q ->
            (set code i (Graphic (Text (e1, e2, Num_expr e3)));
            aux q (i+1))
          | _ ->
            fail t i "Compilation error: Text expects two numerical expressions
              and a string expression as parameters")
      
      | "SGPH1" :: t
      | "SGPH2" :: t
      | "SGPH3" :: t ->
        let (j,t') = process_sgph i lexlist code mem in
        aux t' j
      
      (* Implemented: *)
      | "CLS" :: t
      | "DRAWSTAT" :: t
      | "BGNONE" :: t
      | "AXESON" :: t
      | "AXESOFF" :: t
      | "SLNORMAL" :: t
      | "SLTHICK" :: t
      | "SLBROKEN" :: t
      | "SLDOT" :: t
      (* Ignored: *)
      | "CLRGRAPH" :: t
      | "SWINDMAN" :: t
      | "SWINDAUTO" :: t
      | "LABELON" :: t
      | "LABELOFF" :: t
      | "GRIDON" :: t
      | "GRIDOFF" :: t
      | "COORDON" :: t
      | "COORDOFF" :: t
      | "FUNCON" :: t
      | "FUNCOFF" :: t
      | "DEG" :: t
      | "RAD" :: t
      | "GRA" :: t ->
        (set code i (Graphic (Graphic_Function (List.hd lexlist, [])));
        aux t (i+1))

      | "RCLPICT" :: t
      | "STOPICT" :: t
      | "BGPICT" :: t
      | "RCLCAPT" :: t ->
        let (e, t') = extract_expr t in
        (match List.hd lexlist, t' with
          (* RclCapt is ignored if not followed by Disp.
            If it is present in the compiled code, it means it must be displayed *)
          | "RCLCAPT", "DISP"::q ->
            (set code i (Graphic (Graphic_Function ("RCLCAPT", [e])));
            aux q (i+1))
          | "RCLCAPT", [] -> aux [] i
          | s, _ ->
            (set code i (Graphic (Graphic_Function (s, [e])));
            aux t' (i+1)))

      | "RCLVWIN" :: t
      | "STOVWIN" :: t ->
        let (e, t') = extract_expr t in
        (set code i (Graphic (Graphic_Function (List.hd lexlist, [e])));
        aux t' (i+1))
      
      | ("GRAPHYEQ" as s) :: t
      | ("GRAPHYG" as s) :: t
      | ("GRAPHYL" as s) :: t
      | ("GRAPHYGEQ" as s) :: t
      | ("GRAPHYLEQ" as s) :: t
      | ("GRAPHREQ" as s) :: t
      | ("GRAPHXYEQ" as s) :: t
      | ("GRAPHXEQ" as s) :: t
      | ("GRAPHXG" as s) :: t
      | ("GRAPHXL" as s) :: t
      | ("GRAPHXGEQ" as s) :: t
      | ("GRAPHXLEQ" as s) :: t ->
        let graph_var = s.[5] in (* X, Y, R *)
        let l = String.length s in
        let graph_type = String.sub s 6 (l-6) in (* EQ, G, L, GEQ or LEQ *)
        let (e, t') = extract_expr t in
        (set code i (Graphic (Graph (graph_var, graph_type, [e])));
        aux t' (i+1))

      | "GRAPHS" :: t ->
        let (el, t') = extract_list_content t in
        let len = List.length el in
        if len = 3 then
          (set code i (Graphic (Graph ('S', "", el)));
          aux t' (i+1))
        else
          fail t i "Compilation error: GraphS expects 3 parameters"

      | "MENU" :: t ->
        (let args, t' = extract_line t in
        mem.menustack  <- (i, args) :: mem.menustack;
        aux t' (i+1))

      | "FILE" :: t ->
        let (e, t') = extract_expr t in
        (set code i (Function ("FILE", [e]));
        aux t' (i+1))

      (* Errors *)
      | lex :: _ -> fail lexlist i ("Compilation error: Unexpected command "^(String.escaped lex))

      (* End case *)
      | [] -> i+1 (* Return value *)
  in

  let rec aux_ignore_error (lexlist : string list) (name : string) (t : string list) (i : int) : int =
    try
      aux t i
    with
      | Compilation_error (t', j, error_message) ->
        (if verbose then
          print_error_info lexlist name (List.length t') error_message;
        let (_, t'') = extract_line t' in
        aux_ignore_error lexlist name t'' j)
  in

  (* Called on all programs *)
  (* Compiles the program and returns the next index *)
  let compile_prog i (name,lexlist) =
    mem.progindex <- (name,i)::mem.progindex;
    for k = 0 to Array.length mem.lblindex - 1 do
      mem.lblindex.(k) <- -1
    done;
    mem.gotoindex <- [];
    mem.stack <- [];

    if verbose then
      (print_string "Compiling program "; print_endline name);

    (* Compilation of the program *)
    let j =
      if ignore_errors
        then aux_ignore_error lexlist name lexlist i
        else
          try
            aux lexlist i
          with
            | Compilation_error (t, i, error_message) ->
              (* Handling of compilation error *)
              (print_error_info lexlist name (List.length t) error_message;
              failwith "Compilation aborted")
    in

    (* Post-treatment *)

    set code (j-1) End;
    (* The Goto that were encountered before their labels are set. *)
    List.iter (fun (a_j,k) -> set code k (Goto (mem.lblindex.(a_j)))) mem.gotoindex;
    (* The missing IfEnd are closed. *)
    (* Raises a Compilation_error exception if a keyword different from "if" or "else" is encountered,
      or if mem.stack only contains "break" pointers *)
    while mem.stack <> [] do
      let _ = process_ifend (j-1) [] code mem in ()
    done;
    (* Now that the label indices are set, the menu commands can be processed *)
    List.iter
      (fun (i, args) ->
        try
          process_menu i (List.rev args) code mem
        with
          | Compilation_error (t', _, error_message) ->
            (if verbose then
              print_error_info lexlist name (List.length t') error_message;
            if not ignore_errors then
              failwith "Compilation aborted"))
      mem.menustack;
    mem.menustack <- [];

    (* Next index *)
    j
  in

  (* Compilation of all the programs in the project *)
  let _ = List.fold_left compile_prog 0 prog in
  mem.progindex;;

(* Compiles the list of lexemes lexlist into an object of type basic_code *)
let compile (proglist : program list)
  (verbose : bool) (display_all : bool) (ignore_errors : bool) : basic_code =
  
  let code = ref (Array.make 50 Empty) in
  let prog_index = process_commands code proglist verbose display_all ignore_errors in
  if verbose then
    print_endline "Compilation complete";
  (extract_non_empty !code, prog_index);;