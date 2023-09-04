(* Parsing and evaluation of arithmetical expressions, done at runtime *)
(* #use "basic_parsing/basic_type.ml" *)


(* Subtleties to consider:
  - In Casio Basic, some functions (like Abs, sin...) do not need parentheses.
    They are considered prefix unary operators here.
  - Some functions have a built-in "(". They are parsed as FUNC LPAR and the RPAR is present somewhere in the program.
  - For functions with variable arity (Solve...), we can add several versions with the same name, and choose
    according to the number of commas on the operator queue.
  - The parentheses are not always closed.
*)


    
(** Evaluation **)

(* Returns the value of the variable of index i in the array var *)
let get_var_val (var : float array) (i : int) : complex =
  get_complex var.(i) var.(i+29);;

(* Returns the value of List a[i] *)
let get_list_val (tlist : (bool * (float array)) array) (a : int) (i : int) : complex =
  let (b, l) = tlist.(a) in
  if b
    then get_complex l.(i) l.(i+(Array.length l)/2)
    else complex_of_float l.(i);;

(* Returns the value of Mat a[i1][i2] *)
let get_mat_val (tmat : (bool * (float array array)) array) (a : int) (i1 : int) (i2 : int) : complex =
  let (b, m) = tmat.(a) in
  if b
    then get_complex m.(i1).(i2) m.(i1+(Array.length m)/2).(i2)
    else complex_of_float m.(i1).(i2);;

(* General value getter *)
let get_val (p : parameters) (a : basic_number) : complex =
  match a with
    | Value z -> z
    | Variable (Var a) -> get_var_val p.var a
    | _ -> {re = 0.; im = 0.}
    (* Temporary: to be completed *)
;;


(* All evaluation functions are mutually recursive *)

(* Returns the value of the basic_number n, being a value or a variable *)
let rec get_val (p : parameters) (n : basic_number) : complex =
  match n with
    | Value z -> z
    | Variable (Var i) -> get_var_val p.var i
    | Variable Getkey -> complex_of_float 0. (* To do *)
    | Variable (ListIndex (a,e)) ->
      let z = eval p e in
      (if not (is_int z)
        then failwith "get_val: access to List from an index that is not an integer";
      get_list_val p.proj.list (int_of_complex (get_val p a)) (int_of_complex z))
    | Variable (MatIndex (a,e1,e2)) ->
      let z1 = eval p e1 in
      let z2 = eval p e2 in
      (if not ((is_int z1) && (is_int z2))
        then failwith "get_val: access to Mat from an index that is not an integer";
      get_mat_val p.proj.mat (int_of_complex (get_val p a)) (int_of_complex z1) (int_of_complex z2))
    | Variable Random -> complex_of_float (Random.float 1.)

(* Final evaluation of the arithmetic formula *)
(* p is the parameter container, containing the variables, lists and matrices *)
and calculate (p : parameters) (outq : basic_number list) (opq : arithm list) : complex =
  match outq, opq with
    (* Right parentheses may be omitted at the end of expressions in Casio Basic *)
    | _, Lpar::opqt -> calculate p outq opqt
    | x2::x1::t, (Op o)::opqt -> calculate p ((Value (apply_op o (get_val p x1) (get_val p x2)))::t) opqt
    | x::t, (Lunop lo)::opqt -> calculate p ((Value (apply_lop lo (get_val p x)))::t) opqt
    | [x], [] -> get_val p x
    | _ -> failwith "calculate: Syntax error"

(* Calculates the result of a sequence of right-associative operations
  (ex: 1+2^2^2 is reduced to 1+16) *)
and right_reduce p output_q op_q =
  match (output_q,op_q) with
    | _, []
    | _, Comma::_
    | _, Lpar::_ -> (output_q, op_q)
    | x2::x1::outq, (Op o)::opqt ->
      if left_assoc o
        then (output_q, op_q)
        else right_reduce p ((Value (apply_op o (get_val p x1) (get_val p x2)))::outq) opqt
    | _, (Op o)::_ -> failwith ("reduce: Not enough operands for operator "^o)
    | x::outq, (Lunop lo)::opqt ->
        right_reduce p ((Value (apply_lop lo (get_val p x)))::outq) opqt
    | _ -> failwith "reduce: Syntax error"


(*** Functions will soon be adapted to the new format: Function of string * (basic_expr list) ***)

(* Shunting_yard algorithm: returns the value of the expression *)
(* p is the parameter container, containing the variables, lists and matrices *)
and shunting_yard (p : parameters) (lexlist : arithm list) (output_q : basic_number list) (op_q : arithm list) : complex =
  match (lexlist,op_q) with
    (* End case *)
    | [], _ -> calculate p output_q op_q

    (* Add to a queue *)
    (* Case of omitted multiplication operator *)
    | (Number x1)::(Number x2)::t, _ -> shunting_yard p ((Op "TIMES")::(Number x2)::t) (x1::output_q) op_q
    (* Normal number case *)
    | (Number x)::t, _ -> shunting_yard p t (x::output_q) op_q
    | (Function (fname, pl))::t, _ -> shunting_yard p t output_q ((Function fname)::op_q)
    | Lpar::t, _ -> shunting_yard p t output_q (Lpar::op_q)
    | (Lunop lo)::t, _ -> shunting_yard p t output_q ((Lunop lo)::op_q)

    (* Runop *)
    | (Runop ro)::t, _ ->
      (match output_q with
        | x::outq -> shunting_yard p t ((Value (apply_rop ro (get_val p x)))::outq) op_q
        | _ -> failwith ("No operand for operator "^ro))

    (* Comma *)
    | Comma::t, (Op o)::opqt ->
      if left_assoc o
        then (* Associative or left-associative *)
          (match output_q with
            | x2::x1::outq ->
              shunting_yard p t ((Value (apply_op o (get_val p x1) (get_val p x2)))::outq) (Comma::opqt)
            | _ -> failwith ("Not enough operands for operator "^o))
        else (* Right-associative *)
          let noutq, nopq = right_reduce p output_q op_q in
          shunting_yard p t noutq (Comma::nopq)
    | Comma::t, (Lunop lo)::opqt ->
      let noutq, nopq = right_reduce p output_q op_q in
      shunting_yard p t noutq (Comma::nopq)
    | Comma::t, _ -> shunting_yard p t output_q (Comma::op_q)

    (* Op *)
    | (Op o1)::t, (Op o2)::opqt ->
      if precedence o2 o1 = 1 || (precedence o2 o1 = 0 && left_assoc o1)
        then if left_assoc o2
          then
            (match output_q with
              | x2::x1::outq ->
                shunting_yard p t ((Value (apply_op o2 (get_val p x1) (get_val p x2)))::outq) ((Op o1)::opqt)
              | _ -> failwith ("Not enough operands for operator "^o2))
          else
            let noutq, nopq = right_reduce p output_q op_q in
            shunting_yard p t noutq ((Op o1)::nopq)
        else shunting_yard p t output_q ((Op o1)::op_q)
    | (Op o)::t, (Lunop lo)::opqt ->
      let noutq, nopq = right_reduce p output_q op_q in
      shunting_yard p t noutq ((Op o)::nopq)
    | (Op o)::t, _ -> shunting_yard p t output_q ((Op o)::op_q)
    
    (* Rpar *)
    | Rpar::t, _::_ ->
      (match output_q, op_q with
          (* Function evaluation *)
          | x1::outq, Lpar::(Function fname)::opqt ->
            let af = arity fname in
            if af = 1
              then shunting_yard p t ((Value (apply_func fname [get_val p x1]))::outq) opqt
              else failwith ("Function "^fname^" has arity "^(string_of_int af)^", but receives 1 argument")
          | x2::x1::outq, Comma::Lpar::(Function fname)::opqt ->
            let af = arity fname in
            if af = 2
              then shunting_yard p t ((Value (apply_func fname (List.map (get_val p) [x1;x2])))::outq) opqt
              else failwith ("Function "^fname^" has arity "^(string_of_int af)^", but receives 2 arguments")
          | x3::x2::x1::outq, Comma::Comma::Lpar::(Function fname)::opqt ->
            let af = arity fname in
            if af = 3
              then shunting_yard p t ((Value (apply_func fname (List.map (get_val p) [x1;x2;x3])))::outq) opqt
              else failwith ("Function "^fname^" has arity "^(string_of_int af)^", but receives 3 arguments")
          | x4::x3::x2::x1::outq, Comma::Comma::Comma::Lpar::(Function fname)::opqt ->
            let af = arity fname in
            if af = 4
              then shunting_yard p t ((Value (apply_func fname (List.map (get_val p) [x1;x2;x3;x4])))::outq) opqt
              else failwith ("Function "^fname^" has arity "^(string_of_int af)^", but receives 4 arguments")
          | _, Comma::_ -> failwith "Unexpected comma (maximum arity is 4)"
          
          (* Lpar without a function behind *)
          | _, Lpar::opqt -> shunting_yard p t output_q opqt

          (* Operator evaluation *)
          | x::outq, (Lunop lo)::opqt ->
            shunting_yard p (Rpar::t) ((Value (apply_lop lo (get_val p x)))::outq) opqt
          | x2::x1::outq, (Op o)::opqt ->
            shunting_yard p (Rpar::t) ((Value (apply_op o (get_val p x1) (get_val p x2)))::outq) opqt
          | _, (Op o)::opqt -> failwith ("Not enough operands for operator "^o)

          (* Errors *)
          | _, [] -> failwith "Mismatched parentheses"
          
          | _ -> failwith "Untreated case")    
    | _,_ -> failwith "Syntax error"

(* General arithmetic evaluation function *)
(* p is the parameter container, containing the variables, lists and matrices *)
and eval (p : parameters) (e : basic_expr) : complex =
  match e with
    | Arithm al -> shunting_yard p al [] []
    | _ -> failwith "eval: error, QMark provided";;


(* The question of the unification of functions and list_index parsing is also
  asked in arithmetic_lexing.
  When lexing a List i[e], the expression e is extracted and placed in the constructor ListIndex (i,e)
  when lexing a function f(e1,...,ek), the function is simply added to the queue, with the brackets and commas.
  Do we need to unify these approaches?
  Would it be beneficial (and easy?)to extract the expressions e1,...,ek and put them in a constructor
  Function((fname : string), (el : basic_expr list))?

  Also, the parsing of parameters will also serve in functions like Locate, View-Window, list parsing (see below)...
  So we should simplify the lexing of parameters separated by commas.
*)

(* To do: List/Mat arithmetic
  2*(3+{1,2,3}), evaluates as {8,10,12} 
  It should be unified with complex arithmetic, because it works in the exact same way.
  
  Maybe we should just encapsulate the complex type in a numerical entity type, along with list and mat *)


(* - We could also code all functions with LISTS of parameters
   -> easier to lex (unified with lists)
   -> allows functions of all arities
   -> allows functions of variable arity
   -> simplifies the code
 *)

(* Conclusion: we should handle lists (of arguments, of elements, etc.) in the lexing part.
  Then we can delete the "Comma" arithmetic constructor. *)