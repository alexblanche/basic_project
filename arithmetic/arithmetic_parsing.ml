(* Parsing and evaluation of arithmetical expressions, done at runtime *)

(* Subtleties to consider:
  - In Casio Basic, some functions (like Abs, sin...) do not need parentheses.
    They are considered prefix unary operators here.
  - Some functions have a built-in "(". They are parsed as FUNC (without the LPAR), and the RPAR is present somewhere in the program.
  - The parentheses are not always closed.
*)
    
(** Evaluation **)

(* Returns the value of the variable of index i in the array var *)
let get_var_val (var : float array) (i : int) : complex =
  get_complex var.(i) var.(i+29);;

(* Returns the value of List a[i] *)
let get_list_val (tlist : (float array array)) (a : int) (i : int) : complex =
  let l = tlist.(a) in
  let n = Array.length l in
  get_complex l.(i) l.(i+n/2);;

(* Returns the value of Mat a[i1][i2] *)
let get_mat_val (tmat : (float array array array)) (a : int) (i1 : int) (i2 : int) : complex =
  let m = tmat.(a) in
  let n = Array.length m in
  get_complex m.(i1).(i2) m.(i1+n/2).(i2);;


(* All evaluation functions are mutually recursive *)

(* Returns the value of the basic_number n, being a value or a variable *)
let rec get_val (p : parameters) (n : basic_number) : complex =
  match n with
    | Value z -> z
    | Variable (Var i) -> get_var_val p.var i
    | Variable Getkey -> complex_of_int !getkey

    | Variable (ListIndex (a,e)) ->
      let z = eval p e in
      (if not (is_int z)
        then failwith "get_val: access to List from an index that is not an integer";
      get_list_val p.list (int_of_complex (get_val p a)) (int_of_complex z))
    | Variable (MatIndex (a,e1,e2)) ->
      let z1 = eval p e1 in
      let z2 = eval p e2 in
      (if not ((is_int z1) && (is_int z2))
        then failwith "get_val: access to Mat from an index that is not an integer";
      get_mat_val p.mat (int_of_complex (get_val p a)) (int_of_complex z1) (int_of_complex z2))
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
    | _, Lpar::_ -> (output_q, op_q)
    | x2::x1::outq, (Op o)::opqt ->
      if left_assoc o
        then (output_q, op_q)
        else right_reduce p ((Value (apply_op o (get_val p x1) (get_val p x2)))::outq) opqt
    | _, (Op o)::_ -> failwith ("reduce: Not enough operands for operator "^o)
    | x::outq, (Lunop lo)::opqt ->
        right_reduce p ((Value (apply_lop lo (get_val p x)))::outq) opqt
    | _ -> failwith "reduce: Syntax error"

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

    (* Function evaluation *)
    | (Function (fname, el))::t, _ ->
      let vl = List.map (fun e -> eval p e) el in
      shunting_yard p t ((Value (apply_func fname vl))::output_q) op_q

    | Lpar::t, _ -> shunting_yard p t output_q (Lpar::op_q)
    
    (* Lunop *)
    | (Lunop lo)::t, _ -> shunting_yard p t output_q ((Lunop lo)::op_q)

    (* Runop *)
    | (Runop ro)::t, _ ->
      (match output_q with
        | x::outq -> shunting_yard p t ((Value (apply_rop ro (get_val p x)))::outq) op_q
        | _ -> failwith ("Arithmetic parsing: No operand for operator "^ro))

    (* Op *)
    | (Op o1)::t, (Op o2)::opqt ->
      if precedence o2 o1 = 1 || (precedence o2 o1 = 0 && left_assoc o1)
        then if left_assoc o2
          then
            (match output_q with
              | x2::x1::outq ->
                shunting_yard p t ((Value (apply_op o2 (get_val p x1) (get_val p x2)))::outq) ((Op o1)::opqt)
              | _ -> failwith ("Arithmetic parsing: Not enough operands for operator "^o2))
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
          | _, Lpar::opqt -> shunting_yard p t output_q opqt

          (* Operator evaluation *)
          | x::outq, (Lunop lo)::opqt ->
            shunting_yard p (Rpar::t) ((Value (apply_lop lo (get_val p x)))::outq) opqt
          | x2::x1::outq, (Op o)::opqt ->
            shunting_yard p (Rpar::t) ((Value (apply_op o (get_val p x1) (get_val p x2)))::outq) opqt
          | _, (Op o)::opqt -> failwith ("Arithmetic parsing: Not enough operands for operator "^o)

          (* Errors *)
          | _, [] -> failwith "Arithmetic parsing: Mismatched parentheses"
          
          | _ -> failwith "Arithmetic parsing: Untreated case")    
    | _,_ -> failwith "Arithmetic parsing: Syntax error"

(* General arithmetic evaluation function *)
(* p is the parameter container, containing the variables, lists and matrices *)
and eval (p : parameters) (e : basic_expr) : complex =
  match e with
    | Arithm al -> shunting_yard p al [] []
    | _ -> failwith "eval: error, QMark provided";;


(* To do: List/Mat arithmetic
  2*(3+{1,2,3}), evaluates as {8,10,12} 
  It should be unified with complex arithmetic, because it works in the exact same way.
  
  Maybe we should just encapsulate the complex type in a numerical entity type, along with list and mat *)