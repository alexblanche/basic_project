(* Lexing of arithmetical expressions, done at compilation time *)

(** Useful functions for functions and operators  **)

(* Returns the arity (number of arguments) of the function with name fname *)
let arity (fname : string) =
  try
    (match Hashtbl.find func_table fname with
      | LOP _
      | AR1 _ -> 1
      | AR2 _ -> 2
      | AR3 _ -> 3
      | AR4 _ -> 4
    )
  with
    | Not_found -> failwith ("apply_func: Function "^fname^" undefined");;

(* Returns true if the operator is associative or left-associative *)
let left_assoc (s : string) : bool =
  (s <> "POWER");;

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
  (c >= 65 && c <= 90)
  || c = 205 || c = 206);;

(* Returns true if the string s contains exactly one character that is a digit *)
let is_digit (s : string) : bool =
  (String.length s = 1)
  &&
  (s.[0] >= '0' && s.[0] <= '9');;

(* Returns the index of the variables a (A..Z, r, theta, Ans)
  the variable array used in basic_run.ml
  A..Z = 0..25, r = 26, theta = 27, Ans = 28 *)
let var_index (a : string) : int =
  if a = "SMALLR" then 26
  else if a = "THETA" then 27
  else if a = "ANS" then 28
  else (Char.code a.[0]) - 65;;

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
      | [] -> failwith "read_float: Missing '.' or 'e'"
  in
  let (exp_part, t'') =
    match t' with
      | e::s::q ->
        if e = "TIMESTENPOWER"
          then
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

(* Converts a list of lexemes containing an arithmetic expression into a list of arithmetic lexemes *)
(* Return the basic_expr and the tail of the list of lexemes after the expression *)
let rec extract_expr (lexlist : string list) : basic_expr * (string list) =
  (* Specific case for List i[e] *)
  let extract_list_index t =
    let (i,t') = read_int t true in
    match t' with
      | "LSQBRACKET"::t'' ->
        let (e,t''') = extract_expr t'' in
        (match t''' with
          | "RSQBRACKET"::_
          | "EOL"::_
          | "DISP"::_ -> ((Number (Variable (ListIndex (i, e)))),t''')
          | _ -> failwith "extract_expr: Syntax error, List '[' not properly closed")
      | _ -> failwith "extract_expr: Syntax error, List should be followed by '['"
  in

  (* Specific case for Mat i[e1][e2] *)
  let extract_mat_index t =
    let (i,t2) = read_int t true in
    match t2 with
      | "LSQBRACKET"::t2 ->
        let (e1,t3) = extract_expr t2 in
        (match t3 with
          | "RSQBRACKET"::"LSQBRACKET"::t4 ->
            let (e2,t5) = extract_expr t4 in
            (match t5 with
            | "RSQBRACKET"::_
            | "EOL"::_
            | "DISP"::_ -> ((Number (Variable (ListIndex (i, e)))),t''')
            | _ -> failwith "extract_expr: Syntax error, Mat '[' not properly closed")
          | _ -> failwith "extract_expr: Syntax error, Mat '[' not properly closed")
      | _ -> failwith "extract_expr: Syntax error, Mat should be followed by '['"
  in

  (* Main loop *)
  let rec aux acc l =
    match l with
      | s::t ->
        if is_digit s then
          let (x, t') = read_float s false in
          aux ((Number (Float x))::acc) t'
        else if is_var s || s = "ANS" then
          aux ((Variable (Var (var_index s)))::acc) t
        else if s = "LPAR" then
          aux (Lpar::acc) t
        else if s = "RPAR" then
          aux (Rpar::acc) t
        else if s = "COMMA" then
          aux (Comma::acc) t
        else if List.exists (fun (op,_) -> s=op) op_list then
          aux ((Op s)::acc) t
        else if List.mem s lop_list then
          aux ((Lunop name)::acc) t
        else if List.mem s rop_list then
          aux ((Runop name)::acc) t
        else if s = "LIST" then
          let (li,t') = extract_list_index t in
          aux (li::acc) t'
        else if s = "MAT" then
          let (li,t') = extract_list_index t in
          aux (li::acc) t'
        else if Hashtbl.mem func_table s then
          aux ((Function s)::acc) t
        else (acc, t)
      | [] -> (acc, [])
  in
  match lexlist with
    | "QMARK"::t -> (QMark,t)
    | _ ->
      let (sl, t) = aux [] lexlist in
      (Expr (List.rev sl), t)
  ;;

(* Remark:
  For List i[e] and Mat i[e1][e2], I call recursively extract_expr for each argument,
  but for functions I pass them to the evaluator to parse and compute them.
  Should it be unified? *)