(* Parsing of arithmetical expressions *)

(** Types **)

(* Type for functions, of arity 1 to 4 *)
(* Greater arity is not needed for Casio Basic *)
type funct =
  | LOP of (float -> float)
  | AR1 of (float -> float)
  | AR2 of (float -> float -> float)
  | AR3 of (float -> float -> float -> float)
  | AR4 of (float -> float -> float -> float -> float)

(* Type for arithmetic expressions lexemes *)
type lexeme = Float of float | Lpar (* ( *) | Rpar (* ) *) |
  Op of string | (* Binary operator *)
  Runop of string | (* Left unary operator *)
  Lunop of string | (* Right unary operator *)
  Function of string |
  Comma (* , *)

(* To come:
  - Unify the lexing between ops and {lops, rops, functions}?
  - Treat variables (-> read_name has to read a string,
    and the classification (variable, op, lop, rop, function) is done afterward) *)

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

(* (* Fast exponentiation *)
let rec exp x n =
  if n = 0
    then 1
    else let y = exp x (n/2) in
      if n mod 2 = 0
        then y*y
        else x*y*y;; *)

(* Table of handled functions *)
(* Each function is stored as a pair (a,f),
  where a is the arity of the function (number of arguments) *)

(* Example. The actual exhaustive table will be defined in another file. *)
let func_table =
  let t = Hashtbl.create 10 in
  let func_list = [
    ("max", AR2 (fun a b -> max a b));
    ("f", AR1 (fun x -> x*.x+.2.));
    ("max3", AR3 (fun a b c -> max (max a b) c));
    ("fact", AR1 (fun x -> float_of_int (fact (int_of_float x))));
    ("Abs", LOP Float.abs)
    ]
  in
  List.iter (fun (fname, fdef) -> Hashtbl.add t fname fdef) func_list;
  t;;

(* List of handled left unary operators *)
let lop_list = ["Abs"];;

(* List of handled right unary operators *)
let rop_list = ["!"];;

(* List of handled operators and their index of precedence (1 = greatest *)
let op_list = [("+", 3); ("-", 3); ("*", 2); ("/", 2); ("^", 1);
  ("<=", 4); ("<", 4); (">=", 4); (">", 4); ("=", 4); ("!=", 4);
  ("And", 5); ("Or", 6); ("Xor", 7)];;


(** Useful functions for functions and operators  **)

(* Returns the arity (number of arguments) of the function with name fname *)
let arity (fname : string) =
  try
    (match Hashtbl.find func_table fname with
      | LOP _ -> 1
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
      | Not_found -> failwith ("precedence: Unkown operator "^o2))
  with
    | Not_found -> failwith ("precedence: Unkown operator "^o1);;

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
let apply_func (fname : string) (xl : float list) =
  try
    (match (Hashtbl.find func_table fname, xl) with
      | LOP f, [x1] -> f x1
      | LOP _, _ -> failwith ("apply_func: Operator "^fname^" has a wrong number of arguments")
      | AR1 f, [x1] -> f x1
      | AR2 f, [x1;x2] -> f x1 x2
      | AR3 f, [x1;x2;x3] -> f x1 x2 x3
      | AR4 f, [x1;x2;x3;x4] -> f x1 x2 x3 x4
      | _ -> failwith ("apply_func: Function "^fname^" has a wrong number of arguments")
    )
  with
    | Not_found -> failwith ("apply_func: Function "^fname^" undefined");;

let float_of_bool (b : bool) : float =
  if b then 1. else 0.;;

(* Application of the operators *)
let apply_op (o : string) (x1 : float) (x2 : float) : float =
  (* Arithmetic *)
  if o = "+" then x1 +. x2
  else if o = "-" then x1 -. x2
  else if o = "*" then x1 *. x2
  else if o = "/" then x1 /. x2
  else if o = "^" then x1 ** x2
  (* Relations *)
  else if o = "<=" then float_of_bool (x1 <= x2)
  else if o = "<" then float_of_bool (x1 < x2)
  else if o = ">=" then float_of_bool (x1 >= x2)
  else if o = ">" then float_of_bool (x1 > x2)
  else if o = "=" then float_of_bool (x1 = x2)
  else if o = "!=" then float_of_bool (x1 <> x2)
  (* Logic *)
  else if o = "And" then float_of_bool ((x1 <> 0.) && (x2 <> 0.))
  else if o = "Or" then float_of_bool ((x1 <> 0.) || (x2 <> 0.))
  else if o = "Xor" then
    float_of_bool ((x1 <> 0.) && (x2 = 0.) || (x1 = 0.) && (x2 <> 0.))
  else failwith ("apply_op: Unkown operator "^o);;

(* Application of the right unary operators *)
(* Since there are only a few, we hard-code them like the operators. *)
let apply_rop (ro : string) (x : float) : float =
  if ro = "!"
    then float_of_int (fact (int_of_float x))
    else failwith ("apply_rop: Unkown operator "^ro);;

(* Application of the left unary operators *)
let apply_lop (lo : string) (x : float) : float =
  apply_func lo [x];;


(* Final evaluation of the arithmetic formula *)
let rec calculate (outq : float list) (opq : lexeme list) : float =
  match outq, opq with
    (* Left parentheses left open are allowed in Casio Basic *)
    | _, Lpar::opqt -> calculate outq opqt
    | x2::x1::t, (Op o)::opqt -> calculate ((apply_op o x1 x2)::t) opqt
    | x::t, (Lunop lo)::opqt -> calculate ((apply_lop lo x)::t) opqt
    | [x], [] -> x
    | _ -> failwith "calculate: Syntax error"



(** Main lexing function **)

(* Used for custom Basic interpretation *)
(* For actual Casio Basic, the lexing is done straight from the binary. *)

(* Reads an int at position i of string s
  Returns the int and the new reading position *)
(* If pos is true, the integer read is non-negative;
  otherwise, a '-' may be the first character. *)
let read_int (s : string) (i : int) (pos : bool) : int * int =
  let n = String.length s in
  let rec aux j res =
    if j = n || s.[j] < '0' || s.[j] > '9'
      then (res, j)
      else aux (j+1) (10*res + (int_of_char (s.[j])-48))
  in
  if i = n || pos && s.[i] = '-'
    then (0,i)
    else
      if s.[i] = '-'
        then
          let (res, j) = aux (i+1) 0 in
          (-res, j)
        else aux i 0;;

(* Reads a float at position i of string s
  Returns the float and the new reading position *)
let read_float (s : string) (i : int) : float * int =
  let n = String.length s in
  let (_, i2) = read_int s i false in
  if i2 = n
    then failwith "read_float: Missing '.'"
    else
      let i3 =
        if s.[i2] = '.'
          then let (_, j) = read_int s (i2+1) true in j
          else i2
      in
      let finali =
        if i3<n && s.[i3] = 'e'
          then
            if s.[i3+1] = '+'
              then let (_,j) = read_int s (i3+2) true in j
              else let (_,j) = read_int s (i3+1) false in j
          else i3
      in
      (float_of_string (String.sub s i (finali-i)), finali);;

(* Reads a function name at position i of string s
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
      else aux_read_name (i+1);;

(* Converts a string containing an arithmetic expression into a list of lexemes *)
let lexer (s : string) : lexeme list =
  let n = String.length s in
  let rec aux i acc =
    if i = n then List.rev acc
    else if s.[i] = ' ' then aux (i+1) acc
    else if s.[i] = '(' then aux (i+1) (Lpar::acc)
    else if s.[i] = ')' then aux (i+1) (Rpar::acc)
    else if s.[i] = ',' then aux (i+1) (Comma::acc)
    else if s.[i] >= '0' && s.[i] <= '9' then (* Float *)
      let (res,ni) = read_float s i in
      aux ni ((Float res)::acc)
    else (* Function or operators *)
      let (name,ni) = read_name s i in
      if List.exists (fun (s,_) -> s=name) op_list then aux ni ((Op name)::acc)
      else if List.mem name lop_list then aux ni ((Lunop name)::acc)
      else if List.mem name rop_list then aux ni ((Runop name)::acc)
      else aux ni ((Function name)::acc)
  in
  aux 0 [];;


    
(** Evaluation **)

(* Calculates the result of a sequence of right-associative operations
  (ex: 1+2^2^2 is reduced to 1+16) *)
let rec right_reduce output_q op_q =
  match (output_q,op_q) with
    | _, []
    | _, Comma::_
    | _, Lpar::_ -> (output_q, op_q)
    | x2::x1::outq, (Op o)::opqt ->
      if left_assoc o
        then (output_q, op_q)
        else right_reduce ((apply_op o x1 x2)::outq) opqt
    | _, (Op o)::_ -> failwith ("reduce: Not enough operands for operator "^o)
    | x::outq, (Lunop lo)::opqt ->
        right_reduce ((apply_lop lo x)::outq) opqt
    | _ -> failwith "reduce: Syntax error";;

(* Shunting_yard algorithm: returns the value of the expression *)
let rec shunting_yard (lexlist : lexeme list) (output_q : float list) (op_q : lexeme list) : float =
  match (lexlist,op_q) with
    (* End case *)
    | [], _ -> calculate output_q op_q

    (* Add to a queue *)
    | (Float x)::t, _ -> shunting_yard t (x::output_q) op_q
    | (Function fname)::t, _ -> shunting_yard t output_q ((Function fname)::op_q)
    | Lpar::t, _ -> shunting_yard t output_q (Lpar::op_q)
    | (Lunop lo)::t, _ -> shunting_yard t output_q ((Lunop lo)::op_q)

    (* Runop *)
    | (Runop ro)::t, _ ->
      (match output_q with
        | i::outq -> shunting_yard t ((apply_rop ro i)::outq) op_q
        | _ -> failwith ("No operand for operator"^ro))

    (* Comma *)
    | Comma::t, (Op o)::opqt ->
      if left_assoc o
        then (* Associative or left-associative *)
          (match output_q with
            | x2::x1::outq -> shunting_yard t ((apply_op o x1 x2)::outq) (Comma::opqt)
            | _ -> failwith ("Not enough operands for operator "^o))
        else (* Right-associative *)
          let noutq, nopq = right_reduce output_q op_q in
          shunting_yard t noutq (Comma::nopq)
    | Comma::t, (Lunop lo)::opqt ->
      let noutq, nopq = right_reduce output_q op_q in
      shunting_yard t noutq (Comma::nopq)
    | Comma::t, _ -> shunting_yard t output_q (Comma::op_q)

    (* Op *)
    | (Op o1)::t, (Op o2)::opqt ->
      if precedence o2 o1 = 1 || (precedence o2 o1 = 0 && left_assoc o1)
        then if left_assoc o2
          then
            (match output_q with
              | x2::x1::outq -> shunting_yard t ((apply_op o2 x1 x2)::outq) ((Op o1)::opqt)
              | _ -> failwith ("Not enough operands for operator "^o2))
          else
            let noutq, nopq = right_reduce output_q op_q in
            shunting_yard t noutq ((Op o1)::nopq)
        else shunting_yard t output_q ((Op o1)::op_q)
    | (Op o)::t, (Lunop lo)::opqt ->
      let noutq, nopq = right_reduce output_q op_q in
      shunting_yard t noutq ((Op o)::nopq)
    | (Op o)::t, _ -> shunting_yard t output_q ((Op o)::op_q)
    
    (* Rpar *)
    | Rpar::t, _::_ ->
      (match output_q, op_q with
          (* Function evaluation *)
          | x1::outq, Lpar::(Function fname)::opqt ->
            let af = arity fname in
            if af = 1
              then shunting_yard t ((apply_func fname [x1])::outq) opqt
              else failwith ("Function "^fname^" has arity "^(string_of_int af)^", but receives 1 argument")
          | x2::x1::outq, Comma::Lpar::(Function fname)::opqt ->
            let af = arity fname in
            if af = 2
              then shunting_yard t ((apply_func fname [x1;x2])::outq) opqt
              else failwith ("Function "^fname^" has arity "^(string_of_int af)^", but receives 2 arguments")
          | x3::x2::x1::outq, Comma::Comma::Lpar::(Function fname)::opqt ->
            let af = arity fname in
            if af = 3
              then shunting_yard t ((apply_func fname [x1;x2;x3])::outq) opqt
              else failwith ("Function "^fname^" has arity "^(string_of_int af)^", but receives 3 arguments")
          | x4::x3::x2::x1::outq, Comma::Comma::Comma::Lpar::(Function fname)::opqt ->
            let af = arity fname in
            if af = 4
              then shunting_yard t ((apply_func fname [x1;x2;x3;x4])::outq) opqt
              else failwith ("Function "^fname^" has arity "^(string_of_int af)^", but receives 4 arguments")
          | _, Comma::_ -> failwith "Unexpected comma (maximum arity is 4)"
          
          (* Lpar without a function behind *)
          | _, Lpar::opqt -> shunting_yard t output_q opqt

          (* Operator evaluation *)
          | x::outq, (Lunop lo)::opqt -> shunting_yard (Rpar::t) ((apply_lop lo x)::outq) opqt
          | x2::x1::outq, (Op o)::opqt -> shunting_yard (Rpar::t) ((apply_op o x1 x2)::outq) opqt
          | _, (Op o)::opqt -> failwith ("Not enough operands for operator "^o)

          (* Errors *)
          | _, [] -> failwith "Mismatched parentheses"
          
          | _ -> failwith "Untreated case")    
    | _,_ -> failwith "Syntax error";;

let eval (s : string) =
  shunting_yard (lexer s) [] [];;
