(* Parsing and evaluation of arithmetical expressions, done at runtime *)
(* #use "basic_parsing/basic_type.ml" *)

(* To come:
  - Unify the lexing between ops and {lops, rops, functions}?
  - Treat variables (-> read_name has to read a string,
    and the classification (variable, op, lop, rop, function) is done afterward) *)

(* Subtleties to consider:
  - In Casio Basic, some functions (like Abs, sin...) do not need parentheses.
    They are considered prefix unary operators here.
  - Some functions have a built-in "(". They are parsed as FUNC LPAR and the RPAR is present somewhere in the program.
  - For functions with variable arity (Solve...), we can add several versions with the same name, and choose
    according to the number of commas on the operator queue.
  - The parentheses are not always closed.
*)


    
(** Evaluation **)

(* All functions are going to be circular recursive *)

(* Returns the value of the variable of index i in the array var *)
(* let eval_number (var : float array) (proj : project_content) (n : basic_number) =
  match n with
    | Float x -> x
    | Variable (Var i) -> var.(i)
    | Variable (ListIndex (i,e)) -> ... *)
(* Need specific functions to access the value of a list, a matrix, a variable *)


(* Final evaluation of the arithmetic formula *)
let rec calculate (outq : basic_number list) (opq : arithm list) : float =
  match outq, opq with
    (* Left parentheses left open are allowed in Casio Basic *)
    | _, Lpar::opqt -> calculate outq opqt
    | x2::x1::t, (Op o)::opqt -> calculate ((apply_op o (valux1 x2)::t) opqt
    | x::t, (Lunop lo)::opqt -> calculate ((apply_lop lo x)::t) opqt
    | [x], [] -> x
    | _ -> failwith "calculate: Syntax error"

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
let rec shunting_yard (lexlist : arithm list) (output_q : float list) (op_q : arithm list) : float =
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

let eval (e : basic_expr) : float =
  match e with
    | Expr al -> shunting_yard al [] []
    | _ -> failwith "eval: error, QMark provided";;
