(* Lexing of arithmetical expressions, done at compilation time *)

(** Useful functions for functions and operators  **)

(* Returns true if the operator is associative or left-associative *)
(* Remark: in Basic Casio, 2^2^2^2 = 256, so the exponentiation is left-associative,
   unlike in traditional math *)
let left_assoc (s : string) : bool =
  true;;
  (* (s <> "POWER");; *)

(* Relation of precedence of operators *)
(* 1 if o1 has greater precedence than o2
   0 if o1 and o2 have equal precedence
  -1 if o2 has greater precedence than o1 *)
let rec precedence (o1 : string) (o2 : string) : int =
  try
    let p1 = List.assoc o1 op_list in
    (try
      let p2 = List.assoc o2 op_list in
      if p1 < p2
        then 1
        else if p1 = p2
          then 0
          else -1
    with
      | Not_found -> failwith ("precedence: Unkown operator "^o2))
  with
    | Not_found -> failwith ("precedence: Unkown operator "^o1);;

(* Obsolete: will be reused to lex Custom Basic code *)
(* Checks if the string starting at index i of string s is an operator *)
(* let is_operator (s : string) (i : int) : bool =
  let remaining_char = String.length s - i in
  let is_op sop =
    let n = String.length sop in
    (n <= remaining_char) && (String.sub s i n) = sop
  in
  List.exists (fun (sop, _) -> is_op sop) op_list;; *)


(* Returns true if the string s contains exactly one character among A..Z, r, theta *)
let is_var (s : string) : bool =
  (String.length s = 1)
  &&
  (let c = Char.code s.[0] in
  (c >= 65 && c <= 90))
  || s = "SMALLR"
  || s = "THETA";;

(* Returns true if the string s contains exactly one character that is a digit *)
let is_digit (s : string) : bool =
  (String.length s = 1)
  &&
  (s.[0] >= '0' && s.[0] <= '9');;

(* Returns the index of the variables a (A..Z, r, theta, Ans)
  the variable array used in basic_run.ml
  A..Z = 0..25, r = 26, theta = 27, Ans = 28 *)
let var_index (a : string) : int =
  match a with
    | "SMALLR" -> 26
    | "THETA" -> 27
    | "ANS" -> 28
    | _ -> (Char.code a.[0]) - 65;;

(** Main lexing function **)

(* Reads the next int in the list of lexemes lexlist
  Returns the int and the tail of the list of lexemes after the int *)
(* If pos is true, the integer read is non-negative;
  otherwise, a '-' may be the first character. *)
let read_int (lexlist : string list) (pos : bool) : int * (string list) =
  let rec aux res l =
    match l with
      | [] -> (res, [])
      | a::t ->
        if is_digit a
          then aux (10*res + (int_of_char (a.[0])-48)) t
          else (res, l)
  in
  match lexlist with
    | [] -> (0, [])
    | a::t ->
      if a = "MINUS" || a = "UMINUS"
        then
          if pos
            then (0, lexlist)
            else
              let (res, t') = aux 0 t in
              (-res, t')
        else aux 0 lexlist;;

(* Reads the next float in the list of lexemes lexlist
  Returns the float and the tail of the list of lexemes after the float *)
let read_float (lexlist : string list) : float * (string list) =
  let (int_part, t) = read_int lexlist false in
  let (dec_part, t') =
    match t with
      | dot::q ->
        if dot = "."
          then read_int q true
          else (0, t)
      | [] -> (0, [])
  in
  let (exp_part, t'') =
    match t' with
      | e::s::q ->
        if e = "TIMESTENPOWER" then
          if s = "PLUS"
            then read_int q true
            else read_int (s::q) false
        else (0, t')
      | _ -> (0, t')
  in
  let ints = string_of_int int_part in
  let decs =
    if dec_part = 0
      then ""
      else ("."^string_of_int dec_part)
  in
  let exps =
    if exp_part = 0
      then ""
      else ("e"^string_of_int exp_part)
  in
  (float_of_string (ints^decs^exps), t'');;

(* Obsolete *)
(* (* Reads a function name at position i of string s
  Returns the name and the new reading position *)
let read_name (s : string) (i : int) : (string * int) =
    let n = String.length s in
    let rec aux_read_name j =
      if j = n ||
        s.[j] = ',' ||
        s.[j] = ')' ||
        s.[j] = '(' ||
        s.[j] = ' ' ||
        is_operator s j
        then (String.sub s i (j-i), j)
        else aux_read_name (j+1)
    in
    if is_operator s i
      then
        let (sop, _) =
          List.find (fun (sname,_) -> sname = String.sub s i (String.length sname)) op_list
        in
        (sop, i+String.length sop)
      else aux_read_name (i+1);; *)


(* Auxiliary function to extract a string, simpler than the one from compile_aux *)
(* Returns the string in reverse order in string list form and the tail of the list of lexemes *)
let aux_extract_str (lexlist : string list) : string list * string list =
  let rec aux acc l =
    match l with
      | "QUOTE" :: t -> (acc, t)
      | s::"QUOTE"::t ->
        if s = "\092" (* anti-slash *)
          then aux ("\034"::acc) t (* The quote is kept *)
          else (s::acc, t)
      | s :: t -> aux (s::acc) t
      | [] -> failwith "aux_extract_str: the string is not closed properly"
  in
  aux [] lexlist;;




(* Expression extraction functions are mutually recursive *)
    
(* Extracts the content of List i[e] from the lexlist when lexlist is the tail
  of a list of lexemes starting with "LIST" *)
let rec extract_list_index (t : string list) : arithm * (string list) =
  (* Detection of the first variable *)
  let (a,t') =
    match t with
      | a::q ->
        if is_var a || a = "ANS" then
          (Variable (Var (var_index a)), q)
        else if is_digit a then
          let (i,q') = read_int t true in
          (Value (complex_of_int i), q')
        else failwith "extract_list_index: List should be followed by a number or a variable"
      | [] -> failwith "extract_list_index: Error, the list of lexemes is empty"
  in
  (* Detection of the index between square brackets *)
  match t' with
    | "LSQBRACKET"::t'' ->
      let (e, expr_type, t''') = extract_expr t'' in
      if expr_type <> Numerical
        then failwith "extract_list_index: numerical expression expected"
      else
        let x = Entity (Variable (ListIndex (a, e))) in
        (match t''' with
          | "RSQBRACKET"::q -> (x, q)
          | "EOL"::_ (* Closing bracket may be omitted in Basic Casio *)
          | "COLON"::_
          | "IMPL"::_
          | "ASSIGN"::_
          | ","::_
          | "DISP"::_ -> (x, t''')
          | _ -> failwith "extract_list_index: Syntax error, List '[' not properly closed")
    | _ -> failwith "extract_list_index: Syntax error, List should be followed by '['"

(* Extracts the content of Mat i[e1,e2] from the lexlist when lexlist is the tail
  of a list of lexemes starting with "MAT" *)
and extract_mat_index (t : string list) : arithm * (string list) =
  (* Detection of the first variable *)
  let (ai,t2) =
    match t with
      | a::q ->
        if is_var a && a <> "SMALLR" && a <> "THETA" then
          (var_index a, q)
        else failwith "extract_mat_index: Mat should be followed by a"
      | [] -> failwith "extract_mat_index: Error, the list of lexemes is empty"
  in
  (* Detection of the indices between square brackets *)
  match t2 with
    | "LSQBRACKET"::t2 ->
      let (e1, expr_type1, t3) = extract_expr t2 in
      (match t3 with
        | ","::t4 ->
          let (e2, expr_type2, t5) = extract_expr t4 in
          if expr_type1 <> Numerical || expr_type2 <> Numerical
            then failwith "extract_mat_index: numerical expression expected"
          else
            let x = Entity (Variable (MatIndex (ai, e1, e2))) in
            (match t5 with
            | "RSQBRACKET"::q -> (x, q)
            | "EOL"::_
            | "COLON"::_
            | "IMPL"::_
            | "ASSIGN"::_
            | ","::_
            | "DISP"::_ -> (x, t5)
            | _ -> failwith "extract_mat_index: Syntax error, Mat '[' not properly closed")
        | _ -> failwith "extract_mat_index: Syntax error, Mat '[' not properly closed")
    | _ -> failwith "extract_mat_index: Syntax error, Mat should be followed by '['"

(* Return the list of expressions separated by commas at the beginning of lexlist
  and ending with RPAR, RBRACKET, RSQBRACKET, EOL, DISP or ASSIGN *)
and extract_list_content (lexlist : string list) : (basic_expr * expression_type) list * string list =
  let rec aux acc l =
    match l with
      | ","::t ->
        let (e, expr_type, t') = extract_expr t in
        aux ((e, expr_type)::acc) t'
      
      | "RPAR"::t
      | "RBRACKET"::t
      | "RSQBRACKET"::t -> (List.rev acc, t)
      
      | "EOL"::_
      | "COLON"::_
      | "DISP"::_
      | "IMPL"::_
      | "ASSIGN"::_ -> (List.rev acc, l)

      | [] -> (List.rev acc, [])
      
      | _ ->
        let (e, expr_type, t') = extract_expr l in
        aux ((e, expr_type)::acc) t'
  in
  aux [] lexlist

(* Returns the content of the matrix at the beginning of lexlist *)
and extract_mat_content (lexlist : string list) : num_expr array array * string list =
  let rec aux acc l =
    match l with
      | "LSQBRACKET"::t -> (* [...]...], beginning of a new line *)
        let (el,t') = extract_list_content t in
        aux (el::acc) t'

      (* ], end of the matrix definition *)
      | "RSQBRACKET"::t -> (acc, t)
      (* Omitted ] *)
      | "EOL"::_
      | "COLON"::_
      | "DISP"::_
      | "ASSIGN"::_ -> (acc, l)
      | _ -> failwith "extract_mat_content: syntax error in matrix definition"
  in
  let (ell, t) = aux [] lexlist in
  let row = List.length ell in
  if row = 0
    then failwith "extract_mat_content: empty matrix"
  else
    let l1 = List.hd ell in
    let col = List.length l1 in
    if col = 0
      then failwith "extract_mat_content: empty matrix"
    else
      let m = Array.make_matrix row col (Arithm []) in
      (List.iteri
        (fun i el ->
          if List.length el <> col
            then failwith "extract_mat_content: lines have different length"
          else
            List.iteri
              (fun j (x,expr_type) ->
                if expr_type = Numerical
                  then m.(row-1-i).(j) <- x
                  else failwith "extract_mat_content: one expression is not numerical")
              el)
        ell;
      (m, t))


(* Return the expression within LPAR RPAR *)
and extract_par_content (lexlist : string list) : basic_expr * (string list) =
  let (e,_,t) = extract_expr lexlist in
  match t with
    | s::t' ->
      if s = "RPAR" then
        (e,t')
      else if s = "EOL" || s = "COLON" || s = "DISP" || s = "ASSIGN" || s = "IMPL" then
        (e,t)
      else
        failwith "Arithmetic lexing: Syntax error, unclosed parenthesis"
    | [] -> (e,[])

(** Lexing string expressions **)

(* Auxiliary function to extract the arguments of a string function *)
and aux_extract_str_args (lexlist : string list) : string_expr list * string list =
  let rec aux acc l =
    match l with
      | "," :: t ->
        let (se, t') = extract_string_expr t in
        aux (se::acc) t'

      | "RPAR" :: t -> (List.rev acc, t)
      | "EOL" :: t
      | "COLON" :: t
      | "DISP" :: t
      | "IMPL" :: t
      | "ASSIGN" :: t -> (List.rev acc, l)

      | [] -> (List.rev acc, [])

      | _ ->
        let (se, t) = extract_string_expr l in
        aux (se::acc) t
  in
  aux [] lexlist

(* Returns the string expression and the tail of the list of lexemes *)
and extract_string_expr (lexlist : string list) : string_expr * string list =
  let rec aux l =
    match l with
      | "QUOTE" :: t ->
        let (sl, t') = aux_extract_str t in
        (Str_content sl, t')
      | "STR" :: a :: b :: t ->
        if String.length a = 1 && a >= "0" && a <= "9" then
          if String.length b = 1 && b >= "0" && b <= "9"
            then (Str_access (10*((Char.code a.[0])-48) + (Char.code b.[0])-48-1), t)
            else (Str_access ((Char.code a.[0])-48-1), (b::t))
        else failwith "extract_string_expr: wrong index for string access"
      | "STR" :: [a] ->
        if String.length a = 1 && a >= "0" && a <= "9"
          then (Str_access ((Char.code a.[0])-48-1), [])
          else failwith "extract_string_expr: wrong index for string access"
      | s :: t ->
        if List.mem s string_func_list then
          let (args, t') = aux_extract_str_args t in
          (Str_Func (s, args), t')
        else
          let (e, expr_type, t') = extract_expr l in
          if expr_type = Numerical then
            (Num_expr e, t')
          else failwith "extract_string_expr: wrong type for numerical expression"
      | _ -> (Num_expr (Arithm []), [])
  in
  (* Handling of the '+' operation, that concatenates two non-numerical string expressions *)
  let (se, t) = aux lexlist in
  match (se, t) with
    | Str_content _, "PLUS"::t'
    | Str_access _, "PLUS"::t' ->
      let (se2, t'') = extract_string_expr t' in
      (Str_Func ("STRJOIN", [se; se2]), t'')
    | Str_Func (fname, _), "PLUS"::t' ->
      if List.mem fname numerical_string_functions then
        (* Numerical return value, '+' is the arithmetic operation *)
        (se, t)
      else
        (* String return value, '+' is the concatenation operation *)
        let (se2, t'') = extract_string_expr t' in
        (Str_Func ("STRJOIN", [se; se2]), t'')
    | Num_expr _, _ ->
      (se, t)
    | _ -> (se, t)



(** General arithmetic lexing function **)

(* Converts a list of lexemes containing an arithmetic expression into a list of arithmetic lexemes *)
(* Return the basic_expr and the tail of the list of lexemes after the expression *)
and extract_expr (lexlist : string list) : basic_expr * expression_type * (string list) =

  let rec aux acc expr_type l =
    match l with
      | s::t ->
        (* Values and variables *)
        if is_digit s || s = "." then
          let (x, t') = read_float l in
          aux ((Entity (Value (complex_of_float x)))::acc) expr_type t'
        else if is_var s || s = "ANS" || s = "SMALLR" || s = "THETA" then
          aux ((Entity (Variable (Var (var_index s))))::acc) expr_type t
        else if s = "CPLXI" then
          aux ((Entity (Value (Complex.i)))::acc) expr_type t
        else if s = "PI" then
          aux ((Entity (Value {re = Float.pi; im = 0.}))::acc) expr_type t
        else if s = "GETKEY" then
          aux ((Entity (Variable Getkey))::acc) expr_type t

        (* Parentheses *)
        else if s = "LPAR" then
          let (e,t') = extract_par_content t in
          (match e with
            | Arithm al -> aux (Rpar::(List.rev_append (Lpar::al) acc)) expr_type t'
            | Complex z -> aux (Rpar::(Entity (Value z))::Lpar::acc) expr_type t'
            | StringExpr (Str_Func (fname, sel)) ->
              aux (Rpar::(Function (fname, List.map (fun se -> StringExpr se) sel))::Lpar::acc) expr_type t'
            | _ -> failwith "Arithmetic lexing: syntax error, wrong type in parentheses")

        (* Unary and binary operators *)
        else if List.exists (fun (op,_) -> s=op) op_list then
          (* One exception: Op "MINUS" should be parsed as Lunop "UMINUS" if not preceded by an entity  *)
          if s = "MINUS" then
            (match acc with
              | []
              | Lpar :: _
              | Lunop _ :: _
              | Function _ :: _ -> aux ((Lunop "UMINUS")::acc) expr_type t
              | _ -> aux ((Op "MINUS")::acc) expr_type t)
          else aux ((Op s)::acc) expr_type t
          
        else if List.mem s lop_list then
          aux ((Lunop s)::acc) expr_type t
        else if List.mem s rop_list then
          aux ((Runop s)::acc) expr_type t

        (* Arithmetic function *)
        else if Hashtbl.mem func_table s then
          let (el, t') = extract_list_content t in
          let checked_el =
            List.map
              (fun (e,expr_t) ->
                if expr_t <> Numerical
                  then failwith "extract_expr: list contains another list or a matrix"
                  else e)
              el
          in
          aux ((Function (s, checked_el))::acc) expr_type t'

        (* Entity function *)
        else if Hashtbl.mem entity_func_table s then
          let (el, t') = extract_list_content t in
          aux ((Function (s, List.map fst el))::acc) expr_type t'
        
        (* List a[e] and List a *)
        else if s = "LIST" then
          (match t with
            | _::"LSQBRACKET"::_
            | _::"0"::"LSQBRACKET"::_
            | _::"1"::"LSQBRACKET"::_
            | _::"2"::"LSQBRACKET"::_
            | _::"3"::"LSQBRACKET"::_
            | _::"4"::"LSQBRACKET"::_
            | _::"5"::"LSQBRACKET"::_
            | _::"6"::"LSQBRACKET"::_
            | _::"7"::"LSQBRACKET"::_
            | _::"8"::"LSQBRACKET"::_
            | _::"9"::"LSQBRACKET"::_ -> (* List a[e] *)
              let (li,t') = extract_list_index t in
              aux (li::acc) expr_type t'
            | a::t' -> (* List a *)
              if expr_type = MatExpr
                then failwith "extract_expr: lists and matrices are incompatible"
              else if is_var a || a = "ANS" then
                aux ((Entity (VarList (Variable (Var (var_index a)))))::acc) ListExpr t'
              else if is_digit a then
                let (i,q) = read_int t true in
                aux ((Entity (VarList (Value (complex_of_int i))))::acc) ListExpr q
              else failwith "extract_expr: wrong list index"
            | _ -> failwith "extract_expr: syntax error in list access")
        
        (* Mat a[e1,e2] and Mat a *)
        else if s = "MAT" then
          (match t with
            | _::"LSQBRACKET"::_ -> (* Mat a[e1,e2] *)
              let (mi,t') = extract_mat_index t in
              aux (mi::acc) expr_type t'
            | a::t' -> (* Mat a *)
              if expr_type = ListExpr
                then failwith "extract_expr: matrices and lists are incompatible"
              else if is_var a && a <> "SMALLR" && a <> "THETA" || a = "ANS" then
                aux ((Entity (VarMat (var_index a)))::acc) MatExpr t'
              else failwith "extract_mat_index: wrong matrix index"
            | _ -> failwith "extract_expr: syntax error in matrix access")
        
        (* {...} *)
        else if s = "LBRACKET" then
          (if expr_type = MatExpr
            then failwith "extract_expr: lists and matrices are incompatible"
          else
            let (el, t') = extract_list_content t in
            let checked_el =
              List.map
                (fun (e,expr_t) ->
                  if expr_t <> Numerical
                    then failwith "extract_expr: list contains another list or a matrix"
                    else e)
                el
            in
            aux ((Entity (ListContent (Array.of_list checked_el)))::acc) ListExpr t')

        (* [[...]...[...]] *)
        else if s = "LSQBRACKET" then
          (if expr_type = ListExpr
            then failwith "extract_expr: matrices and lists are incompatible"
          else
            let (m, t') = extract_mat_content t in
            aux ((Entity (MatContent m))::acc) MatExpr t')

        (* String functions (with numerical return value) *)
        else if List.mem s numerical_string_functions then
          let (se, t') = extract_string_expr (s::t) in
          (match se with
            | Str_Func (_, sel) ->
              aux ((Function (s, List.map (fun se -> StringExpr se) sel))::acc) expr_type t'
            | _ -> failwith "extract_expr: error in lexing of string function")
        
        (* End of the expression *)
        else (acc, expr_type, l)
      | [] -> (acc, expr_type, [])
  in

  match lexlist with
    | "QMARK"::t -> (QMark, Numerical, t)
    | _ ->
        let (sl, expr_type, t) = aux [] Numerical lexlist in
        if List.length sl = 1 && expr_type = Numerical
          then
            (match sl with
              | [Entity (Value z)] -> (Complex z, Numerical, t)
              | _ -> (Arithm sl, Numerical, t))
          else (Arithm (List.rev sl), expr_type, t);;

(* Specific lexing functions that check the return type *)
let extract_num_expr (lexlist : string list) : num_expr * (string list) =
  let (e, expr_type, t) = extract_expr lexlist in
  if expr_type = Numerical
    then (e,t)
    else failwith "extract_num_expr: numerical expression expected";;

let extract_list_expr (lexlist : string list) : list_expr * (string list) =
  let (e, expr_type, t) = extract_expr lexlist in
  if expr_type = ListExpr
    then (e,t)
    else failwith "extract_list_expr: list expression expected";;

let extract_mat_expr (lexlist : string list) : mat_expr * (string list) =
  let (e, expr_type, t) = extract_expr lexlist in
  if expr_type = MatExpr
    then (e,t)
    else failwith "extract_mat_expr: mat expression expected";;