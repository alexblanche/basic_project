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

(* Returns the value of the variable of index i in the array var *)
let get_var_val (var : float array) (i : int) : float =
  var.(i);;

(* Returns the value of List a[i] *)
let get_list_val (p : project_content) (a : int) (i : int) : float =
  let (b, l) = p.list.(a) in
  l.(i);;

(* Returns the value of Mat a[i1][i2] *)
let get_mat_val (p : project_content) (a : int) (i1 : int) (i2 : int) : float =
  let (b, m) = p.mat.(a) in
  m.(i1).(i2);;

(* All evaluation functions are mutually recursive *)

(* Returns the value of the variable of index i in the array var *)
let rec get_val (var : float array) (p : project_content) (n : basic_number) : float =
  match n with
    | Float x -> x
    | Variable (Var i) -> get_var_val var i
    | Variable Getkey -> 0. (* To do *)
    | Variable (ListIndex (a,e)) ->
      let x = eval var p e in
      (if not (Float.is_integer x)
        then failwith "get_val: access to List from an index that is not an integer";
      get_list_val p a (Float.to_int x))
    | Variable (MatIndex (a,e1,e2)) ->
      let x1 = eval var p e1 in
      let x2 = eval var p e2 in
      (if not ((Float.is_integer x1) && (Float.is_integer x2))
        then failwith "get_val: access to Mat from an index that is not an integer";
      get_mat_val p a (Float.to_int x1) (Float.to_int x2))
    | Variable Random -> Random.float 1.

(* Final evaluation of the arithmetic formula *)
(* var, p are the variable content array and the project_content containing the lists and matrices *)
and calculate (var : float array) (p : project_content) (outq : basic_number list) (opq : arithm list) : float =
  match outq, opq with
    (* Left parentheses left open are allowed in Casio Basic *)
    | _, Lpar::opqt -> calculate var p outq opqt
    | x2::x1::t, (Op o)::opqt -> calculate var p ((Float (apply_op o (get_val var p x1) (get_val var p x2)))::t) opqt
    | x::t, (Lunop lo)::opqt -> calculate var p ((Float (apply_lop lo (get_val var p x)))::t) opqt
    | [x], [] -> get_val var p x
    | _ -> failwith "calculate: Syntax error"

(* Calculates the result of a sequence of right-associative operations
  (ex: 1+2^2^2 is reduced to 1+16) *)
and right_reduce var p output_q op_q =
  match (output_q,op_q) with
    | _, []
    | _, Comma::_
    | _, Lpar::_ -> (output_q, op_q)
    | x2::x1::outq, (Op o)::opqt ->
      if left_assoc o
        then (output_q, op_q)
        else right_reduce var p ((Float (apply_op o (get_val var p x1) (get_val var p x2)))::outq) opqt
    | _, (Op o)::_ -> failwith ("reduce: Not enough operands for operator "^o)
    | x::outq, (Lunop lo)::opqt ->
        right_reduce var p ((Float (apply_lop lo (get_val var p x)))::outq) opqt
    | _ -> failwith "reduce: Syntax error"

(* Shunting_yard algorithm: returns the value of the expression *)
(* var, p are the variable content array and the project_content containing the lists and matrices *)
and shunting_yard (var : float array) (p : project_content)
  (lexlist : arithm list) (output_q : basic_number list) (op_q : arithm list) : float =
  match (lexlist,op_q) with
    (* End case *)
    | [], _ -> calculate var p output_q op_q

    (* Add to a queue *)
    | (Number x)::t, _ -> shunting_yard var p t (x::output_q) op_q
    | (Function fname)::t, _ -> shunting_yard var p t output_q ((Function fname)::op_q)
    | Lpar::t, _ -> shunting_yard var p t output_q (Lpar::op_q)
    | (Lunop lo)::t, _ -> shunting_yard var p t output_q ((Lunop lo)::op_q)

    (* Runop *)
    | (Runop ro)::t, _ ->
      (match output_q with
        | x::outq -> shunting_yard var p t ((Float (apply_rop ro (get_val var p x)))::outq) op_q
        | _ -> failwith ("No operand for operator"^ro))

    (* Comma *)
    | Comma::t, (Op o)::opqt ->
      if left_assoc o
        then (* Associative or left-associative *)
          (match output_q with
            | x2::x1::outq ->
              shunting_yard var p t ((Float (apply_op o (get_val var p x1) (get_val var p x2)))::outq) (Comma::opqt)
            | _ -> failwith ("Not enough operands for operator "^o))
        else (* Right-associative *)
          let noutq, nopq = right_reduce var p output_q op_q in
          shunting_yard var p t noutq (Comma::nopq)
    | Comma::t, (Lunop lo)::opqt ->
      let noutq, nopq = right_reduce var p output_q op_q in
      shunting_yard var p t noutq (Comma::nopq)
    | Comma::t, _ -> shunting_yard var p t output_q (Comma::op_q)

    (* Op *)
    | (Op o1)::t, (Op o2)::opqt ->
      if precedence o2 o1 = 1 || (precedence o2 o1 = 0 && left_assoc o1)
        then if left_assoc o2
          then
            (match output_q with
              | x2::x1::outq ->
                shunting_yard var p t ((Float (apply_op o2 (get_val var p x1) (get_val var p x2)))::outq) ((Op o1)::opqt)
              | _ -> failwith ("Not enough operands for operator "^o2))
          else
            let noutq, nopq = right_reduce var p output_q op_q in
            shunting_yard var p t noutq ((Op o1)::nopq)
        else shunting_yard var p t output_q ((Op o1)::op_q)
    | (Op o)::t, (Lunop lo)::opqt ->
      let noutq, nopq = right_reduce var p output_q op_q in
      shunting_yard var p t noutq ((Op o)::nopq)
    | (Op o)::t, _ -> shunting_yard var p t output_q ((Op o)::op_q)
    
    (* Rpar *)
    | Rpar::t, _::_ ->
      (match output_q, op_q with
          (* Function evaluation *)
          | x1::outq, Lpar::(Function fname)::opqt ->
            let af = arity fname in
            if af = 1
              then shunting_yard var p t ((Float (apply_func fname [get_val var p x1]))::outq) opqt
              else failwith ("Function "^fname^" has arity "^(string_of_int af)^", but receives 1 argument")
          | x2::x1::outq, Comma::Lpar::(Function fname)::opqt ->
            let af = arity fname in
            if af = 2
              then shunting_yard var p t ((Float (apply_func fname (List.map (get_val var p) [x1;x2])))::outq) opqt
              else failwith ("Function "^fname^" has arity "^(string_of_int af)^", but receives 2 arguments")
          | x3::x2::x1::outq, Comma::Comma::Lpar::(Function fname)::opqt ->
            let af = arity fname in
            if af = 3
              then shunting_yard var p t ((Float (apply_func fname (List.map (get_val var p) [x1;x2;x3])))::outq) opqt
              else failwith ("Function "^fname^" has arity "^(string_of_int af)^", but receives 3 arguments")
          | x4::x3::x2::x1::outq, Comma::Comma::Comma::Lpar::(Function fname)::opqt ->
            let af = arity fname in
            if af = 4
              then shunting_yard var p t ((Float (apply_func fname (List.map (get_val var p) [x1;x2;x3;x4])))::outq) opqt
              else failwith ("Function "^fname^" has arity "^(string_of_int af)^", but receives 4 arguments")
          | _, Comma::_ -> failwith "Unexpected comma (maximum arity is 4)"
          
          (* Lpar without a function behind *)
          | _, Lpar::opqt -> shunting_yard var p t output_q opqt

          (* Operator evaluation *)
          | x::outq, (Lunop lo)::opqt ->
            shunting_yard var p (Rpar::t) ((Float (apply_lop lo (get_val var p x)))::outq) opqt
          | x2::x1::outq, (Op o)::opqt ->
            shunting_yard var p (Rpar::t) ((Float (apply_op o (get_val var p x1) (get_val var p x2)))::outq) opqt
          | _, (Op o)::opqt -> failwith ("Not enough operands for operator "^o)

          (* Errors *)
          | _, [] -> failwith "Mismatched parentheses"
          
          | _ -> failwith "Untreated case")    
    | _,_ -> failwith "Syntax error"

(* General arithmetic evaluation function *)
(* var, p are the variable content array and the project_content containing the lists and matrices *)
and eval (var : float array) (p : project_content) (e : basic_expr) : float =
  match e with
    | Arithm al -> shunting_yard var p al [] []
    | _ -> failwith "eval: error, QMark provided";;

(* To do:
  - take care of Variable (GetKey | Random)
  - rewrite the unit tests *)

(* Comments on the advancement of the project:
  The code for the arithmetic parsing is getting really ugly.
  It may need some refactoring to simplify it.
  The case of var, p polluting the whole program is upsetting.

  Major change incoming: complex numbers must be dealt with eventually.
  Do we treat all the numbers like complex numbers? (with the OCaml module Complex)
  (And adapt float_repr to complex numbers)
  Or do we simply add a constructor Complex to basic_number and try to make it work
  in a polymorphic way?

  Also, let us try not to make this program more complicated.
*)

(* The question of the unification of functions and list_index parsing is also
  asked in arithmetic_lexing.
  When lexing a List i[e], the expression e is extracted and placed in the constructor ListIndex (i,e)
  when lexing a function f(e1,...,ek), the function is simply added to the queue, with the brackets and commas.
  Do we need to unify these approaches?
  Would it be beneficial (and easy?)to extract the expressions e1,...,ek and put them in a constructor
  Function((fname : string), (el : basic_expr list))?
*)