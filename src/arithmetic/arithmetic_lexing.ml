(* Lexing of arithmetical expressions, done at compilation time *)

(** Useful functions for functions and operators  **)

(* Returns true if the operator is associative or left-associative *)
(* Remark: in Basic Casio, 2^2^2^2 = 256, so the exponentiation is left-associative,
   unlike in traditional math *)
let left_assoc (s : string) : bool =
  true;;
  (* (s <> "POWER");; *)

(* Relation of precedence of operators *)
(* 1 if o1 has greater precedence than o2 (o1 should be applied first)
   0 if o1 and o2 have equal precedence
  -1 if o2 has greater precedence than o1 (o2 should be applied first) *)
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

(* List of the special functions *)
(* For each function, the associated list gives the arity and indicates to the
  arithmetic expression evaluator which arguments should be evaluated beforehand (true)
  or passed as-is (false) *)
  let special_functions_list =
    [("SEQ", [false; false; true; true; true]);
     ("PXLTEST", [true; true]);
     ("MATTOLIST", [false; true]);
     ("SIGMAPAR", [false; false; true; true])];;

(* Obsolete: will be reused to lex Custom Basic code *)
(* Checks if the string starting at index i of string s is an operator *)
(* let is_operator (s : string) (i : int) : bool =
  let remaining_char = String.length s - i in
  let is_op sop =
    let n = String.length sop in
    (n <= remaining_char) && (String.sub s i n) = sop
  in
  List.exists (fun (sop, _) -> is_op sop) op_list;; *)

(* Returns true if the string s contains exactly one character that is a digit *)
let is_digit (s : string) : bool =
  (String.length s = 1)
  &&
  (s.[0] >= '0' && s.[0] <= '9');;



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

(* Returns the number of zeroes encountered in the list of lexemes lexlist,
   and returns the tail of this list *)
let extract_zeroes (lexlist : string list) : int * (string list) =
  let rec aux n l =
    match l with
      | "0" :: t -> aux (n+1) t
      | _ :: _ -> (n, l)
      | [] -> (n, [])
  in
  aux 0 lexlist;;

(* Reads the next float in the list of lexemes lexlist
  Returns the float and the tail of the list of lexemes after the float *)
let read_float (lexlist : string list) : float * (string list) =
  let (int_part, t) = read_int lexlist false in
  let (dec_part, nb_zeroes, t') =
    match t with
      | dot::q ->
        if dot = "."
          then
            let (nb_z, q') = extract_zeroes q in
            let (d, t') = read_int q' true in
            (d, nb_z, t')
          else (0, 0, t)
      | [] -> (0, 0, [])
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
  let ints =
    match lexlist with
      | "TIMESTENPOWER" :: _ -> "1"
      | _ -> string_of_int int_part
  in
  let decs =
    if dec_part = 0
      then ""
      else ("."^(String.make nb_zeroes '0')^string_of_int dec_part)
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


(* Auxiliary function to extract a string *)
(* Returns the string in reverse order in string list form and the tail of the list of lexemes *)
let aux_extract_str (lexlist : string list) : string list * string list =
  let rec aux acc l =
    match l with
      | "QUOTE" :: t -> (acc, t)
      | "\092"::s::t -> (* anti-slash *)
        if s = "QUOTE" || s = "\092"
          then aux (s::acc) t (* The quote or anti-slash is kept *)
          else aux acc (s::t)
      | "EOL" :: t -> (acc, l)
      | s :: t -> aux (s::acc) t
      (* Strangely, the closing quote can be omitted at the end of the code *)
      | [] -> (acc, [])
  in
  aux [] lexlist;;




(* Expression extraction functions are mutually recursive *)
    
(* Extracts the content of List i[e] from the lexlist when lexlist is the tail
  of a list of lexemes starting with "LIST" *)
let rec extract_list_index (t : string list) : arithm * (string list) =
  (* Detection of the first variable *)
  let (a,t') =
    match t with
      | "QUOTE" :: q ->
        let (sl, q') = aux_extract_str q in
        (StringExpr (Str_content sl), q')
      | a::q ->
        if is_letter_var a || a = "ANS" then
          (Arithm [Entity (Variable (Var (var_index a)))], q)
        else if is_digit a then
          let (i,q') = read_int t true in
          (Arithm [Entity (Value (complex_of_int i))], q')
        else failwith "extract_list_index: List should be followed by a number or a variable"
      | [] -> failwith "extract_list_index: Error, the list of lexemes is empty"
  in
  (* Detection of the index between square brackets *)
  match t' with
    | "LSQBRACKET"::t'' ->
      let (e, t''') = extract_expr t'' in
      let x = Entity (Variable (ListIndex (a, e))) in
      (match t''' with
        | "RSQBRACKET"::q -> (x, q)
        | "EOL"::_ (* Closing bracket may be omitted in Basic Casio *)
        | "COLON"::_
        | "IMPL"::_
        | "ASSIGN"::_
        | ","::_
        | "DISP"::_
        | "STEP"::_
        | [] -> (x, t''')
        | _ -> failwith "extract_list_index: Syntax error, List '[' not properly closed")
    | _ -> failwith "extract_list_index: Syntax error, List should be followed by '['"

(* Extracts the content of Mat i[e1,e2] from the lexlist when lexlist is the tail
  of a list of lexemes starting with "MAT" *)
and extract_mat_index (t : string list) : arithm * (string list) =
  (* Detection of the first variable *)
  let (ai,t2) =
    match t with
      | a::q ->
        if is_letter_var a then
          let vi = var_index a in
          if vi <= 25 || vi = 28 then
            (vi, q)
          else failwith "extract_mat_index: a matrix index variable has to be a letter and neither r nor theta"
        else failwith "extract_mat_index: Mat should be followed by a variable"
      | [] -> failwith "extract_mat_index: Error, the list of lexemes is empty"
  in
  (* Detection of the indices between square brackets *)
  match t2 with
    | "LSQBRACKET"::t2 ->
      let (e1, t3) = extract_expr t2 in
      (match t3 with
        | ","::t4 ->
          let (e2, t5) = extract_expr t4 in
          let x = Entity (Variable (MatIndex (ai, e1, e2))) in
          (match t5 with
            | "RSQBRACKET"::q -> (x, q)
            | "EOL"::_
            | "COLON"::_
            | "IMPL"::_
            | "ASSIGN"::_
            | ","::_
            | "DISP"::_
            | "STEP"::_
            | [] -> (x, t5)
            | _ -> failwith "extract_mat_index: Syntax error, Mat '[' not properly closed")
        | _ -> failwith "extract_mat_index: Syntax error, Mat '[' not properly closed")
    | _ -> failwith "extract_mat_index: Syntax error, Mat should be followed by '['"

(* Return the list of expressions separated by commas at the beginning of lexlist
  and ending with RPAR, RBRACKET, RSQBRACKET, EOL, DISP or ASSIGN *)
and extract_list_content (lexlist : string list) : basic_expr list * string list =
  let rec aux acc l =
    match l with
      | ","::t ->
        (match extract_expr t with
          | Arithm [], _ ->
            (* Not a numerical expression:
              attempt at finding a string expression *)
              (match extract_string_expr t with
                | Num_expr (Arithm []), _ ->
                  (* Not a string expression either *)
                  failwith "extract_list_content: syntax error"
                | se, t' -> aux ((StringExpr se)::acc) t')
          | e, t' -> aux (e::acc) t')
      
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
        let (e, t') = extract_expr l in
        aux (e::acc) t'
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
      | [] -> (acc, [])
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
            List.iteri (fun j x -> m.(row-1-i).(j) <- x) el)
        ell;
      (m, t))


(* Return the expression within LPAR RPAR *)
and extract_par_content (lexlist : string list) : basic_expr * (string list) =
  let (e, t) = extract_expr lexlist in
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
      (* | "LIST" :: t ->
        let (li,t') = extract_list_index t in
        (match li with
            | Entity (Variable (ListIndex (a, e))) ->
              (ListIndexZero (a, e), t')
            | _ -> failwith "extract_string_expr: wrong index of list index zero") *)
      | s :: t ->
        if List.mem s string_func_list && not (List.mem s numerical_string_functions) then
          let (args, t') = aux_extract_str_args t in
          (Str_Func (s, args), t')
        else
          let (e, t') = extract_expr l in
          (Num_expr e, t')
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
and extract_expr (lexlist : string list) : basic_expr * (string list) =

  let rec aux acc l =
    match l with
      | s::t ->
        (* Values and variables *)
        if is_digit s || s = "." || s = "TIMESTENPOWER" then
          let (x, t') = read_float l in
          aux ((Entity (Value (complex_of_float x)))::acc) t'
        else if is_var s || s = "ANS" then
          aux ((Entity (Variable (Var (var_index s))))::acc) t
        else if s = "CPLXI" then
          aux ((Entity (Value (Complex.i)))::acc) t
        else if s = "PI" then
          aux ((Entity (Value {re = Float.pi; im = 0.}))::acc) t
        else if s = "GETKEY" then
          aux ((Entity (Variable Getkey))::acc) t
        else if s = "RAN" then
          aux ((Entity (Variable Random))::acc) t

        (* Parentheses *)
        else if s = "LPAR" then
          let (e,t') = extract_par_content t in
          (match e, t' with
            | Arithm al, "LPAR"::_ -> aux ((Op "OMITTEDTIMES")::Rpar::(List.rev_append (Lpar::al) acc)) t'
            | Arithm al, _ -> aux (Rpar::(List.rev_append (Lpar::al) acc)) t'

            | Complex z, "LPAR"::_ -> aux ((Op "OMITTEDTIMES")::Rpar::(Entity (Value z))::Lpar::acc) t'
            | Complex z, _ -> aux (Rpar::(Entity (Value z))::Lpar::acc) t'
            
            | StringExpr (Str_Func (fname, sel)), "LPAR"::_ ->
              aux ((Op "OMITTEDTIMES")::Rpar::(Function (fname, List.map (fun se -> StringExpr se) sel))::Lpar::acc) t'
            | StringExpr (Str_Func (fname, sel)), _ ->
              aux (Rpar::(Function (fname, List.map (fun se -> StringExpr se) sel))::Lpar::acc) t'
            | _ -> failwith "Arithmetic lexing: syntax error, wrong type in parentheses")

        (* Unary and binary operators *)
        else if List.exists (fun (op,_) -> s=op) op_list then
          (* One exception: Op "MINUS" should be parsed as Lunop "UMINUS" if not preceded by an entity  *)
          if s = "MINUS" then
            (match acc with
              | []
              | Lpar :: _
              | Lunop _ :: _
              | Op _ :: _ -> aux ((Lunop ("UMINUS", true))::acc) t
              | _ -> aux ((Op "MINUS")::acc) t)
          else aux ((Op s)::acc) t
          
        else if List.mem s lop_list then
          aux ((Lunop (s,true))::acc) t
        else if List.mem s rop_list then
          aux ((Runop s)::acc) t

        (* Arithmetic function *)
        else if Hashtbl.mem func_table s then
          let (el, t') = extract_list_content t in
          aux ((Function (s, el))::acc) t'

        (* Entity/special function *)
        else if List.mem s entity_lop_list then
          aux ((Lunop (s,false))::acc) t
        else if Hashtbl.mem entity_func_table s
          || List.mem_assoc s special_functions_list then
          let (el, t') = extract_list_content t in
          aux ((Function (s, el))::acc) t'
        
        (* List a[e] and List a *)
        else if s = "LIST" then
          (match t with
            | "QUOTE" :: q ->
              (* List "..." *)
              let (sl, q') = aux_extract_str q in
              (match q' with
                | "LSQBRACKET" :: _ ->
                  (* List "..."[_] *)
                  (* For safety, we go back and let extract_list_index do the entire extraction *)
                  let (li, t') = extract_list_index t in
                  aux (li::acc) t'
                | _ ->
                  (* List "..." *)
                  aux ((Entity (VarList (StringExpr (Str_content sl))))::acc) q'
              )

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
              aux (li::acc) t'
            | a::t' -> (* List a *)
              if is_letter_var a || a = "ANS" then
                aux ((Entity (VarList (Arithm [Entity (Variable (Var (var_index a)))])))::acc) t'
              else if is_digit a then
                let (i,q) = read_int t true in
                aux ((Entity (VarList (Arithm [Entity (Value (complex_of_int i))])))::acc) q
              else failwith "extract_expr: wrong list index"
            | _ -> failwith "extract_expr: syntax error in list access")
        
        (* Mat a[e1,e2] and Mat a *)
        else if s = "MAT" then
          (match t with
            | _::"LSQBRACKET"::_ -> (* Mat a[e1,e2] *)
              let (mi,t') = extract_mat_index t in
              aux (mi::acc) t'
            | a::t' -> (* Mat a *)
              if is_var a || a = "ANS" then
                let vi = var_index a in
                if vi <= 25 || vi = 28 then
                  aux ((Entity (VarMat (var_index a)))::acc) t'
                else failwith "extract_expr: a matrix index variable has to be a letter and neither r nor theta"
              else failwith "extract_expr: wrong matrix index"
            | _ -> failwith "extract_expr: syntax error in matrix access")
        
        (* {...} *)
        else if s = "LBRACKET" then
          (let (el, t') = extract_list_content t in
          aux ((Entity (ListContent (Array.of_list el)))::acc) t')

        (* [[...]...[...]] *)
        else if s = "LSQBRACKET" then
          (let (m, t') = extract_mat_content t in
          aux ((Entity (MatContent m))::acc) t')

        (* String functions (with numerical return value) *)
        else if List.mem s numerical_string_functions then
          let (sel, t') = aux_extract_str_args t in
          aux ((Function (s, List.map (fun se -> StringExpr se) sel))::acc) t'
        
        (* End of the expression *)
        else (acc, l)
      | [] -> (acc, [])
  in

  match lexlist with
    | "QMARK"::t -> (QMark, t)
    | _ ->
        let (sl, t) = aux [] lexlist in
        if List.length sl = 1 then
          (match sl with
            | [Entity (Value z)] -> (Complex z, t)
            | _ -> (Arithm sl, t))
        else (Arithm (List.rev sl), t);;

(* Specific lexing functions that check the return type *)
let extract_num_expr (lexlist : string list) : num_expr * (string list) =
  extract_expr lexlist;;

let extract_list_expr (lexlist : string list) : list_expr * (string list) =
  extract_expr lexlist;;

let extract_mat_expr (lexlist : string list) : mat_expr * (string list) =
  extract_expr lexlist;;