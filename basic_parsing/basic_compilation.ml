(* Compilation of Basic code *)

(* Compiles the list of lexemes lexlist by modifying the array code in place. *)
(* Returns the proglist parameter of the working memory *)
let process_commands (code : (command array) ref) (prog : ((string * (string list)) list)) : (string * int) list =
  let mem =
    {
      stack = [];
      lblindex = Array.make 28 (-1);
      gotoindex = [];
      progindex = [];
    }
  in

  (* Looping function *)
  
  let rec aux (lexlist : string list) (i : int) : int =

    (* print_endline (string_of_int i); *)

    (* Expression handling *)
    let (e, expr_type, t) = extract_expr lexlist in
    let (expr_found, j, next_t) =
      (match e, t with
      
        | Arithm [], _ ->
          (* The beginning of lexlist is not an expression *)
          (false, -1, [])

        (* To be combined with the case below *)
        | _, "ASSIGN"::t' ->
          (match expr_type, t' with
            | Numerical, [v] ->
              (if is_var v then
                (set code i (Assign (e, Var (var_index v)));
                (true, i+1, []))
              else fail lexlist "Compilation: Wrong assignment (->) of a variable")
            | _, v::"EOL"::t''
            | _, v::"COLON"::t''
            | _, v::"DISP"::t'' ->
              if is_var v && expr_type = Numerical then
                (set code i (Assign (e, Var (var_index v)));
                (true, i+1, List.tl t'))
              else
                fail lexlist "Compilation error: Wrong assignment (->) of a variable"

            | Numerical, "LIST"::_::"LSQBRACKET"::_ ->
              let (li,t'') = extract_list_index (List.tl t') in
              (match li with
                | Entity (Variable lx) ->
                  (set code i (Assign (e, lx));
                  (true, i+1, t''))
                | _ -> fail lexlist "Compilation error: Wrong assignment (->) of a list element")

            | Numerical, "MAT"::_::"LSQBRACKET"::_ ->
              let (mi,t'') = extract_mat_index (List.tl t') in
              (match mi with
                | Entity (Variable mx) ->
                  (set code i (Assign (e, mx));
                  (true, i+1, t''))
                | _ -> fail lexlist "Compilation error: Wrong assignment (->) of a matrix element")

            | ListExpr, "LIST"::a::t'' ->
              if is_var a || a = "ANS" then
                (set code i (AssignList (e, Variable (Var (var_index a))));
                (true, i+1, t''))
              else if is_digit a then
                let (vi,q) = read_int (a::t'') true in
                (set code i (AssignList (e, Value (complex_of_int vi)));
                (true, i+1, t''))
              else fail lexlist  "Compilation error: wrong list index"

            | MatExpr, "MAT"::a::t'' ->
              if is_var a && a <> "SMALLR" && a <> "THETA" || a = "ANS" then
                (set code i (AssignMat (e, var_index a));
                (true, i+1, t''))
              else fail lexlist  "extract_mat_index: wrong matrix index"

            (* Errors *)
            | Numerical, "LIST"::_
            | Numerical, "MAT"::_
            | ListExpr, "MAT"::_
            | MatExpr, "LIST"::_
            | ListExpr, _::_
            | MatExpr, _::_ -> fail lexlist "Compilation error: incompatible types during assignment"
            | _ -> fail lexlist "Compilation error: Syntax error during assignment"
          )
        
        (* QMark is treated in the case above *)
        | QMark, _ -> fail lexlist "Compilation error: Syntax error, ? should be followed by ->"
          
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
            | Compilation_error (_, error_message) -> fail lexlist error_message)
        
        | _, "EOL"::_
        | _, "COLON"::_
        | _, "DISP"::_
        | _, [] ->
          (set code i (Expr (e, Numerical)); (* Simple expression *) (* To do: implement list_expr and mat_expr *)
          (true, i+1, t))

        | _ -> fail lexlist "Compilation error: Syntax error after an expression"
      )
    in
    if expr_found then aux next_t j
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

      (* Strings *)
      | "QUOTE" :: a :: t ->
        let (sl, t') = extract_str (a::t) in
        (match t' with
          | "EOL"::_
          | "DISP"::_
          | "COLON"::_ ->
            (set code i (String sl);
            aux t' (i+1))
          | "ASSIGN"::"STR"::t'' ->
            (let (vi,q) = read_int t'' true in
            set code i (AssignStr (sl, vi-1));
            aux q (i+1))
          | _ -> fail t "Compilation error: Syntax error after a string")

      | "LOCATE" :: t ->
        let (j,t') = process_locate i t code mem in
        aux t' j

      | "DISP" :: t ->
        (set code i Disp;
        aux t (i+1))
      
      | "LBL"::a::eol::t ->
        let (j,t') = process_lbl i t code mem a eol in
        aux t' j
      
      | "GOTO"::a::eol::t ->
        let (j,t') = process_goto i t code mem a eol in
        aux t' j
      
      | "PROG"::"QUOTE"::t ->
        let (j,t') = process_prog i t code mem in
        aux t' j
      
      (* Comment ('): line ignored *)
      | "\039"::t ->
        let (_, t') = extract_line t in
        aux t' i

      | "CLRTEXT" :: eol :: t ->
        (if eol <> "EOL" && eol <> "COLON" && eol <> "DISP" then
          fail (eol :: t) "Compilation error: CLRTEXT should be followed by Eol";
        set code i (Function "CLRTEXT");
        aux t (i+1))

      (* Errors *)
      | lex :: _ -> fail lexlist ("Compilation error: Unexpected command "^(String.escaped lex))

      (* End case *)
      | [] -> i+1 (* Return value *)
  in

  (* Called on all programs *)
  (* Compiles the program and returns the next index *)
  let compile_prog i (name,lexlist) =
    mem.progindex <- (name,i)::mem.progindex;
    for k = 0 to 27 do
      mem.lblindex.(k) <- -1
    done;
    mem.gotoindex <- [];

    try
      (* Compilation of the program *)
      let j = aux lexlist i in

      (* Post-treatment *)

      set code (j-1) End;
      (* The Goto that were encountered before their labels are set. *)
      List.iter (fun (a_j,k) -> set code k (Goto (mem.lblindex.(a_j)))) mem.gotoindex;
      (* The missing IfEnd are closed. *)
      (* Raises a Compilation_error exception if a keyword different from "if" or "else" is encountered *)
      while mem.stack <> [] do
        let _ = process_ifend (j-1) [] code mem in ()
      done;

      (* Next index *)
      j
    with
      | Compilation_error (t, error_message) ->
        (* Handling of compilation error *)
        (let len_t = List.length t in
        let len_lex = List.length lexlist in
        print_endline "---------------------------------------";
        print_endline ("Compilation error in program "^name);
        print_endline error_message;
        print_newline ();
        print_around lexlist (len_lex - len_t);
        print_endline "---------------------------------------";
        failwith "Compilation aborted")
  in

  (* Compilation of all the programs in the project *)
  let _ = List.fold_left compile_prog 0 prog in
  mem.progindex;;

(* Compiles the list of lexemes lexlist into an object of type basic_code *)
let compile (proglist : program list) : basic_code =
  let code = ref (Array.make 50 Empty) in
  let prog_index = process_commands code proglist in
  (extract_non_empty !code, prog_index);;