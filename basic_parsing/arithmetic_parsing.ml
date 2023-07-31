(* Parsing of arithmetical expressions *)

(** Types **)

(* Type for functions, of arity 1 to 4 *)
(* Greater arity is not needed for Casio Basic *)
type funct =
  | AR1 of (int -> int)
  | AR2 of (int -> int -> int)
  | AR3 of (int -> int -> int -> int)
  | AR4 of (int -> int -> int -> int -> int)

(* Type for arithmetic expressions lexemes *)
type lexeme = Int of int | LPAR | RPAR |
  Op of string |
  Function of string |
  COMMA

(* To come:
  prefix unary operators: Not, Uminus, ...
  suffix unary operators: !, ^2, ... *)

(* Subtleties to consider:
  - In Casio Basic, functions (like Abs, sin...) do not need parentheses.
    They are considered prefix unary operators here.
  - Some functions have a built-in "(". They are parsed as FUNC LPAR and the RPAR is present somewhere in the program.
  - For functions with variable arity (Solve...), we can add several versions with the same name, and choose
    according to the number of commas on the operator queue.
  - The parentheses are not always closed.
*)



(** Definitions **)

(* Some functions *)

(* Factorial *)
let fact n =
  let rec aux a i =
    if i = 0
      then a
      else aux (i*a) (i-1)
  in
  aux 1 n;;

(* Fast exponentiation *)
let rec exp x n =
  if n = 0
    then 1
    else let y = exp x (n/2) in
      if n mod 2 = 0
        then y*y
        else x*y*y;;

(* Table of handled functions *)
(* Each function is stored as a pair (a,f),
  where a is the arity of the function (number of arguments) *)

(* Example. The actual exhaustive table will be defined in another file. *)
let func_table =
  let t = Hashtbl.create 10 in
  let func_list = [
    ("max", AR2 (fun a b -> max a b));
    ("f", AR1 (fun x -> x*x+2));
    ("max3", AR3 (fun a b c -> max (max a b) c));
    ("fact", AR1 fact)
    ]
  in
  List.iter (fun (fname, fdef) -> Hashtbl.add t fname fdef) func_list;
  t;;

(* List of handled operators and their index of precedence (1 = greatest *)
let op_list = [("+", 3); ("-", 3); ("*", 2); ("/", 2); ("^", 1);
  ("<=", 4); ("<", 4); (">=", 4); (">", 4); ("=", 4); ("!=", 4);
  ("And", 5); ("Or", 6); ("Xor", 7)];;


(** Useful functions for functions and operators  **)

(* Returns the arity (number of arguments) of the function with name fname *)
let arity (fname : string) =
  try
    (match Hashtbl.find func_table fname with
      | AR1 _ -> 1
      | AR2 _ -> 2
      | AR3 _ -> 3
      | AR4 _ -> 4
    )
  with
    | Not_found -> failwith ("apply_func: Function "^fname^" undefined");;

(* Returns true if the operator is associative or left-associative *)
let left_assoc (s : string) : bool =
  (s <> "^");;

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
      | Not_found -> failwith ("precedence: unkown operator "^o2))
  with
    | Not_found -> failwith ("precedence: unkown operator "^o1);;

(* Checks if the string starting at index i of string s is an operator *)
  let is_operator (s : string) (i : int) : bool =
    let remaining_char = String.length s - i in
    let is_op sop =
      let n = String.length sop in
      (n <= remaining_char) && (String.sub s i n) = sop
    in
    List.exists (fun (sop, _) -> is_op sop) op_list;;


(** Application functions **)

(* Application of the functions *)
let apply_func (fname : string) (il : int list) =
  try
    (match (Hashtbl.find func_table fname, il) with
      | AR1 f, [i1] -> f i1
      | AR2 f, [i1;i2] -> f i1 i2
      | AR3 f, [i1;i2;i3] -> f i1 i2 i3
      | AR4 f, [i1;i2;i3;i4] -> f i1 i2 i3 i4
      | _ -> failwith ("apply_func: Function "^fname^" has a wrong number of arguments")
    )
  with
    | Not_found -> failwith ("apply_func: Function "^fname^" undefined");;

let int_of_bool (b : bool) : int =
  if b then 1 else 0;;

(* Application of the operators *)
let apply_op (o : string) (i1 : int) (i2 : int) : int =
  (* Arithmetic *)
  if o = "+" then i1 + i2
  else if o = "-" then i1 - i2
  else if o = "*" then i1 * i2
  else if o = "/" then i1 / i2
  else if o = "^" then exp i1 i2
  (* Relations *)
  else if o = "<=" then int_of_bool (i1 <= i2)
  else if o = "<" then int_of_bool (i1 < i2)
  else if o = ">=" then int_of_bool (i1 >= i2)
  else if o = ">" then int_of_bool (i1 > i2)
  else if o = "=" then int_of_bool (i1 = i2)
  else if o = "!=" then int_of_bool (i1 <> i2)
  (* Logic *)
  else if o = "And" then int_of_bool ((i1 <> 0) && (i2 <> 0))
  else if o = "Or" then int_of_bool ((i1 <> 0) || (i2 <> 0))
  else if o = "Xor" then
    int_of_bool ((i1 <> 0) && (i2 = 0) || (i1 = 0) && (i2 <> 0))
  else failwith ("apply_op: Unkown operator "^o);;

(* Final evaluation of the arithmetic formula *)
let rec calculate outq opq =
  match outq, opq with
    (* Left parentheses left open are allowed in Casio Basic *)
    | _, LPAR::opqt -> calculate outq opqt
    | i2::i1::t, (Op o)::opqt -> calculate ((apply_op o i1 i2)::t) opqt
    | [i], [] -> i
    | _ -> failwith "calculate: Syntax error"



(** Main lexing function **)

(* Used for custom Basic interpretation *)
(* For actual Casio Basic, the lexing is done straight from the binary. *)

(* Reads an int at position i of string s
  Returns the int and the new reading position *)
let read_int (s : string) (i : int) : int * int =
  let n = String.length s in
  let rec aux j res =
    if j = n || s.[j] < '0' || s.[j] > '9'
      then (res, j)
      else aux (j+1) (10*res + (int_of_char (s.[j])-48))
  in aux i 0;;

(* Reads a function name at position i of string s
  Returns the name and the new reading position *)
let read_name (s : string) (i : int) =
    let n = String.length s in
    let rec aux_read_name j =
      if j = n ||
        s.[j] = ',' ||
        s.[j] = ')' ||
        (is_operator s j)
        then failwith ("Function "^(String.sub s i (j-i))^" has no argument")
        else if s.[j] = '('
          then (String.sub s i (j-i), j)
          else aux_read_name (j+1)
    in
    aux_read_name (i+1);;

(* Converts a string containing an arithmetic expression into a list of lexemes *)
let lexer (s : string) : lexeme list =
  let n = String.length s in
  let rec aux i acc =
    if i = n
      then List.rev acc
      else if is_operator s i
        then aux (i+1) ((Op (String.init 1 (fun _ -> s.[i])))::acc)
        else if s.[i] = '('
          then aux (i+1) (LPAR::acc)
          else if s.[i] = ')'
            then aux (i+1) (RPAR::acc)
            else if s.[i] = ','
              then aux (i+1) (COMMA::acc)
              else if s.[i] >= '0' && s.[i] <= '9'
                then (* Int *)
                  let (res,ni) = read_int s i in
                  aux ni ((Int res)::acc)
                else (* Function *)
                  let (fname,ni) = read_name s i in
                  aux ni ((Function fname)::acc)
  in
  aux 0 [];;


    
(** Evaluation **)

(* Calculates the result of a sequence of right-associative operations
  (ex: 1+2^2^2 is reduced to 1+16) *)
let rec right_reduce output_q op_q =
  match (output_q,op_q) with
    | _, []
    | _, COMMA::_
    | _, LPAR::_ -> (output_q, op_q)
    | i2::i1::outq, (Op o)::opqt ->
      if left_assoc o
        then (output_q, op_q)
        else right_reduce ((apply_op o i1 i2)::outq) opqt
    | _, (Op o)::_ -> failwith ("reduce: Not enough operands for operator"^o)
    | _ -> failwith "reduce: Syntax error";;

(* Shunting_yard algorithm: returns the value of the expression *)
let rec shunting_yard (lexlist : lexeme list) (output_q : int list) (op_q : lexeme list) =
  match (lexlist,op_q) with
    (* End case *)
    | [], _ -> calculate output_q op_q

    (* Add to a queue *)
    | (Int i)::t, _ -> shunting_yard t (i::output_q) op_q
    | (Function fname)::t, _ -> shunting_yard t output_q ((Function fname)::op_q)
    | LPAR::t, _ -> shunting_yard t output_q (LPAR::op_q)

    (* COMMA *)
    | COMMA::t, (Op o)::opqt ->
      if left_assoc o
        then (* Associative or left-associative *)
          (match output_q with
            | i2::i1::outq -> shunting_yard t ((apply_op o i1 i2)::outq) (COMMA::opqt)
            | _ -> failwith ("Not enough operands for operator "^o))
        else (* Right-associative *)
          let noutq, nopq = right_reduce output_q op_q in
          shunting_yard t noutq (COMMA::nopq)
    | COMMA::t, _ -> shunting_yard t output_q (COMMA::op_q)

    (* Op *)
    | (Op o1)::t, (Op o2)::opqt ->
      if precedence o2 o1 = 1 || (precedence o2 o1 = 0 && left_assoc o1)
        then if left_assoc o2
          then
            (match output_q with
              | i2::i1::outq -> shunting_yard t ((apply_op o2 i1 i2)::outq) ((Op o1)::opqt)
              | _ -> failwith ("Not enough operands for operator "^o2))
          else
            let noutq, nopq = right_reduce output_q op_q in
            shunting_yard t noutq ((Op o1)::nopq)
        else shunting_yard t output_q ((Op o1)::op_q)
    | (Op o)::t, _ -> shunting_yard t output_q ((Op o)::op_q)
    
    (* RPAR *)
    | RPAR::t, _::_ ->
      (match output_q, op_q with
          (* Function evaluation *)
          | i1::outq, LPAR::(Function fname)::opqt ->
            let af = arity fname in
            if af = 1
              then shunting_yard t ((apply_func fname [i1])::outq) opqt
              else failwith ("Function "^fname^"has arity "^(string_of_int af)^", but receives 1 argument")
          | i2::i1::outq, COMMA::LPAR::(Function fname)::opqt ->
            let af = arity fname in
            if af = 2
              then shunting_yard t ((apply_func fname [i1;i2])::outq) opqt
              else failwith ("Function "^fname^"has arity "^(string_of_int af)^", but receives 2 arguments")
          | i3::i2::i1::outq, COMMA::COMMA::LPAR::(Function fname)::opqt ->
            let af = arity fname in
            if af = 3
              then shunting_yard t ((apply_func fname [i1;i2;i3])::outq) opqt
              else failwith ("Function "^fname^"has arity "^(string_of_int af)^", but receives 3 arguments")
          | i4::i3::i2::i1::outq, COMMA::COMMA::COMMA::LPAR::(Function fname)::opqt ->
            let af = arity fname in
            if af = 4
              then shunting_yard t ((apply_func fname [i1;i2;i3;i4])::outq) opqt
              else failwith ("Function "^fname^"has arity "^(string_of_int af)^", but receives 4 arguments")
          
          (* LPAR without a function behind *)
          | _, LPAR::opqt -> shunting_yard t output_q opqt

          (* Operator evaluation *)
          | i2::i1::outq, (Op o)::opqt -> shunting_yard (RPAR::t) ((apply_op o i1 i2)::outq) opqt
          | _, (Op o)::opqt -> failwith ("Not enough operands for operator "^o)

          (* Errors *)
          | _, [] -> failwith "Mismatched parentheses"
          | _,COMMA::_ -> failwith "Unexpected comma (maximum arity is 4)"
          | _ -> failwith "Something's wrong")    
    | _,_ -> failwith "Syntax error";;

let eval (s : string) =
  shunting_yard (lexer s) [] [];;
