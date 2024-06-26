(* Parsing and evaluation of arithmetical expressions, done at runtime *)

(* Subtleties to consider:
  - In Casio Basic, some functions (like Abs, sin...) do not need parentheses.
    They are considered prefix unary operators here.
  - Some functions have a built-in "(". They are parsed as FUNC (without the LPAR), and the RPAR is present somewhere in the program.
  - The parentheses are not always closed.
*)

(** Array and matrix representation conversion functions **)
(* The float representation is used when matrices are stored in memory,
  the entity representation is used in the computation of list/mat arithmetic expressions *)

let float_to_numexpr_array (t : float array) : num_expr array =
  let n = Array.length t in
  Array.init (n/2) (fun i -> Complex (get_complex t.(i) t.(i+n/2)));;

let float_to_numexpr_matrix (m : float array array) : num_expr array array =
  let n = Array.length m in
  if n = 0
    then [||]
  else
    let k = Array.length m.(0) in
    let em = Array.make_matrix (n/2) k (Complex {re = 0.; im = 0.}) in
    (for i = 0 to (n/2)-1 do
      for j = 0 to k-1 do
        em.(i).(j) <- Complex (get_complex m.(i).(j) m.(i+n/2).(j))
      done
    done;
    em);;

(* Works only when each element of et is a constructor Complex *)
let numexpr_to_float_array (et : num_expr array) : float array =
  let n = Array.length et in
  let t = Array.make (2*n) 0. in
  Array.iteri
    (fun i e ->
      match e with
        | Complex z ->
          (t.(i) <- z.re;
          t.(i+n) <- z.im)
        | _ -> failwith "numexpr_to_float_array: wrong input")
    et;
  t;;

(* Works only when each element of em is a constructor Complex *)
let numexpr_to_float_matrix (em : num_expr array array) : float array array =
  let n = Array.length em in
  if n = 0
    then [||]
  else
    let k = Array.length em.(0) in
    let m = Array.make_matrix (2*n) k 0. in
    (for i = 0 to n-1 do
      for j = 0 to k-1 do
        match em.(i).(j) with
          | Complex z ->
            (m.(i).(j) <- z.re;
            m.(i+n).(j) <- z.im)
          | _ -> failwith "numexpr_to_float_matrix: wrong input"
      done
    done;
    m);;


(** Evaluation **)

(* Returns the value of the variable of index i in the array var *)
let get_var_val (var : float array) (i : int) : complex =
  (* get_complex var.(i) var.(i+29);; *)
  access_var var i;;

(* Returns the value of List a[i] *)
let get_list_val (tlist : float array array) (listfile : int) (a : int) (i : int) : complex =
  let l = tlist.(26 * listfile + a) in
  let n = Array.length l in
  get_complex l.(i) l.(i+n/2);;

(* Returns the value of Mat a[i1][i2] *)
let get_mat_val (tmat : float array array array) (a : int) (i1 : int) (i2 : int) : complex =
  let m = tmat.(if a = 28 then 26 else a) in
  let n = Array.length m in
  get_complex m.(i1).(i2) m.(i1+n/2).(i2);;

(* Returns the length of n when n is an entity of type list *)
(*
let get_list_length (var : float array) (tlist : float array array) (n : entity) : int =
  match n with
    | ListContent t -> Array.length t
    | VarList (Value z) ->
      Array.length (get_list tlist (int_of_complex z - 1))
    | VarList (Variable (Var vi)) ->
      Array.length (get_list tlist (int_of_complex (get_var_val var vi) - 1))
    | _ -> failwith "get_list_length: n is not a list";; *)

(* Returns the dimensions of n when n is an entity of type matrix *)
let get_mat_dim (tmat : float array array array) (n : entity) : int * int =
  match n with
    | MatContent m ->
      let n = Array.length m in
      if n = 0
        then (0,0)
        else (n, Array.length m.(0))
    | VarMat a ->
      let m = get_mat tmat a in
      let n = Array.length m in
      if n = 0
        then (0,0)
        else (n/2, Array.length m.(0))
    | _ -> failwith "get_mat_dim: n is not a matrix";;


(* All evaluation functions are mutually recursive *)

(* Returns the value of the entity n, when n is a value or a variable *)
let rec get_val_numexpr (p : parameters) (n : entity) : complex =
  match n with
    | Value z -> z
    | Variable (Var i) -> get_var_val p.var i
    | Variable Getkey -> complex_of_int !getkey
    (* List Ans[e], List Ans stored at index 6 * 26 *)

    | Variable (ListIndex (Arithm [Entity (Variable (Var 28))], e)) ->
      let z = eval_num p e in
      (if not (is_int z)
        then failwith "get_val_numexpr: access to List from an index that is not an integer";
      get_list_val p.list p.listfile (6 * 26) (int_of_complex z - 1))

    | Variable (ListIndex (Complex za, e)) ->
      let z = eval_num p e in
      (if not (is_int z)
        then failwith "get_val_numexpr: access to List from an index that is not an integer";
      get_list_val p.list p.listfile (int_of_complex za - 1) (int_of_complex z - 1))

    (* List (value or variable) [e] *)
    | Variable (ListIndex (Arithm [Entity a], e)) ->
      let z = eval_num p e in
      (if not (is_int z)
        then failwith "get_val_numexpr: access to List from an index that is not an integer";
      get_list_val p.list p.listfile (int_of_complex (get_val_numexpr p a) - 1) (int_of_complex z - 1))

    (* List (string) [e] *)
    | Variable (ListIndex (StringExpr (Str_content sl), e)) ->
      let z = eval_num p e in
      (if not (is_int z)
        then failwith "get_val_numexpr: access to List from an index that is not an integer";
      try
        let ai = list_index_from_string p.listfile p.listzero sl in
        get_list_val p.list p.listfile ai (int_of_complex z - 1)
      with
        | Not_found -> failwith "get_val_numexpr: List string index not found")

    | Variable (MatIndex (ai,e1,e2)) ->
      let z1 = eval_num p e1 in
      let z2 = eval_num p e2 in
      (if not ((is_int z1) && (is_int z2))
        then failwith "get_val_numexpr: access to Mat from an index that is not an integer";
      get_mat_val p.mat ai (int_of_complex z1 - 1) (int_of_complex z2 - 1))
    
    | Variable Random -> complex_of_float (Random.float 1.)
    | _ -> failwith "get_val_numexpr: entity is a list or matrix"

(* Returns the value of the entity n, when n is a list *)
(* Evaluates each expression and returns an array of (Complex complex) constructors *)
and get_val_listexpr (p : parameters) (n : entity) : num_expr array =
  match n with
    | ListContent et -> Array.map (fun e -> Complex (eval_num p e)) et
    | VarList (Arithm [Entity (Value z)]) ->
      (if not (is_int z)
        then failwith "get_val_list: access to List from an index that is not an integer";
      float_to_numexpr_array p.list.(26 * p.listfile + int_of_complex z - 1))
    | VarList (Arithm [Entity (Variable (Var vi))]) ->
      let i =
        if vi = 28 (* Ans *)
          then 6 * 26 (* Index of List Ans *)
          else
            let vali = get_var_val p.var vi in (* List _ *)
            if not (is_int vali)
              then failwith "get_val_list: access to List from an index that is not an integer"
              else 26 * p.listfile + int_of_complex vali - 1
      in
      float_to_numexpr_array p.list.(i)
    | VarList (StringExpr (Str_content sl)) ->
      (try
        let ai = list_index_from_string p.listfile p.listzero sl in
        float_to_numexpr_array p.list.(ai)
      with
        | Not_found -> failwith "get_val_list: List string index not found")
    | _ -> failwith "get_val_listexpr: entity is a number or a matrix, or is accessed with a wrong index"

(* Returns the value of the entity n, when n is a matrix *)
(* Evaluates each expression and returns a matrix of (Complex complex) constructors *)
and get_val_matexpr (p : parameters) (n : entity) : num_expr array array =
  match n with
    | MatContent em ->
      let (r,c) = get_mat_dim p.mat n in
      (* (if r=0 || c=0
        then failwith "get_val_matexpr: the matrix is empty"; *)
      let m = Array.make_matrix r c QMark in
      (for i = 0 to r-1 do
        for j = 0 to c-1 do
          m.(i).(j) <- Complex (eval_num p em.(i).(j))
        done
      done;
      m)
    | VarMat ai ->
      if ai = 28 (* Ans *)
        then float_to_numexpr_matrix p.mat.(26) (* Index of Mat Ans *)
        else float_to_numexpr_matrix p.mat.(ai)
    | _ -> failwith "get_val_listexpr: entity is a number or a matrix"

(** Application of the operators to an entity **)
(* Binary operators *)
and apply_op (p : parameters) (o : string) (n1 : entity) (n2 : entity) : entity =
  match n1, n2 with
    | Value z1, Value z2 -> Value (apply_op_single o z1 z2)
    | _ ->
      (match entity_type n1, entity_type n2 with
        (* Two numbers or variables *)
        | Numerical, Numerical ->
          let z1 = get_val_numexpr p n1 in
          let z2 = get_val_numexpr p n2 in
          Value (apply_op_single o z1 z2)

        (* Number and list *)
        | Numerical, ListExpr ->
          let z1 = get_val_numexpr p n1 in (* complex *)
          let et2 = get_val_listexpr p n2 in (* (Complex complex) array *)
          ListContent (Array.map (fun x -> Complex (apply_op_single o z1 (eval_num p x))) et2)          
        | ListExpr, Numerical ->
          let et1 = get_val_listexpr p n1 in (* (Complex complex) array *)
          let z2 = get_val_numexpr p n2 in (* complex *)
          ListContent (Array.map (fun x -> Complex (apply_op_single o (eval_num p x) z2)) et1)

        (* Two lists *)
        | ListExpr, ListExpr ->
          let et1 = get_val_listexpr p n1 in (* (Complex complex) array *)
          let et2 = get_val_listexpr p n2 in (* (Complex complex) array *)
          let n1 = Array.length et1 in
          if Array.length et2 = n1 then
            ListContent (Array.map2 (fun x1 x2 -> Complex (apply_op_single o (eval_num p x1) (eval_num p x2))) et1 et2)
          else failwith "apply_op: the two lists do not have the same length"

        (* Number and matrix *)
        | Numerical, _ ->
          let z1 = get_val_numexpr p n1 in (* complex *)
          let em2 = get_val_matexpr p n2 in (* (Complex complex) array array *)
          let (r,c) = get_mat_dim p.mat n2 in
          let m = Array.make_matrix r c QMark in
          (for i = 0 to r-1 do
            for j = 0 to c-1 do
              m.(i).(j) <- Complex (apply_op_single o z1 (eval_num p em2.(i).(j)))
            done
          done;
          MatContent m)
        | _, Numerical ->
          let em1 = get_val_matexpr p n1 in (* (Complex complex) array array *)
          let z2 = get_val_numexpr p n2 in (* complex *)
          let (r,c) = get_mat_dim p.mat n1 in
          let m = Array.make_matrix r c QMark in
          (for i = 0 to r-1 do
            for j = 0 to c-1 do
              m.(i).(j) <- Complex (apply_op_single o (eval_num p em1.(i).(j)) z2)
            done
          done;
          MatContent m)

        (* Two matrices *)
        | MatExpr, _ ->
          let em1 = get_val_matexpr p n1 in (* (Complex complex) array array *)
          let em2 = get_val_matexpr p n2 in (* (Complex complex) array array *)
          let (r1,c1) = get_mat_dim p.mat n1 in
          let (r2,c2) = get_mat_dim p.mat n2 in
          (if r1<>r2 || c1<>c2
            then failwith "apply_op: the two matrices do not have the same dimensions";
          let m = Array.make_matrix r1 c1 QMark in
          for i = 0 to r1-1 do
            for j = 0 to c1-1 do
              m.(i).(j) <- Complex (apply_op_single o (eval_num p em1.(i).(j)) (eval_num p em2.(i).(j)))
            done
          done;
          MatContent m)

        (* Errors *)
        | _ -> failwith "apply_op: incompatible types (list and matrix)"
      )

(* Left unary operators *)
and apply_lop (p : parameters) (lo : string) (n : entity) (a : bool) : entity =
  if a then
    match entity_type n with
      | Numerical -> Value (apply_lop_single lo (get_val_numexpr p n))
      | ListExpr ->
        let et = get_val_listexpr p n in (* (Complex complex) array *)
        ListContent (Array.map (fun x -> Complex (apply_lop_single lo (eval_num p x))) et)
      | _ ->
        let em = get_val_matexpr p n in (* (Complex complex) array array *)
        let (r,c) = get_mat_dim p.mat n in
        let m = Array.make_matrix r c QMark in
        (for i = 0 to r-1 do
          for j = 0 to c-1 do
            m.(i).(j) <- Complex (apply_lop_single lo (eval_num p em.(i).(j)))
          done
        done;
        MatContent m)
  else apply_entity_func lo [eval_entity p (Arithm [Entity n])]

(* Right unary operators *)
and apply_rop (p : parameters) (ro : string) (n : entity) : entity =
  match entity_type n with
    | Numerical -> Value (apply_rop_single ro (get_val_numexpr p n))
    | ListExpr ->
      let et = get_val_listexpr p n in (* (Complex complex) array *)
      ListContent (Array.map (fun x -> Complex (apply_rop_single ro (eval_num p x))) et)
    | _ ->
      let em = get_val_matexpr p n in (* (Complex complex) array array *)
      let (r,c) = get_mat_dim p.mat n in
      let m = Array.make_matrix r c QMark in
      (for i = 0 to r-1 do
        for j = 0 to c-1 do
          m.(i).(j) <- Complex (apply_rop_single ro (eval_num p em.(i).(j)))
        done
      done;
      MatContent m)

(* String expressions evaluation *)
(* Returns a string_expr of the form Str_content _ or Num_expr (Complex _) (used as arguments of functions) *)
and eval_str (p : parameters) (se : string_expr) : string_expr =
  match se with
    (* Case of a list index: if the index is zero, it is a string, otherwise it is a number *)
    | Num_expr (Arithm [Entity (Variable (ListIndex (ea , ej)))]) ->
      let j = eval_num p ej in
      if is_zero j then
        let a = eval_num p ea in
        Str_content p.listzero.(int_of_complex a - 1)
      else
        Num_expr (Complex (eval_num p (Arithm [Entity (Variable (ListIndex (ea , ej)))])))
    
    | Num_expr e -> Num_expr (Complex (eval_num p e))
    | Str_content _ -> se
    | Str_access si -> Str_content p.str.(si)
    | Str_Func (fname, sel) ->
      apply_str_func p fname (List.map (fun se -> eval_str p se) sel)
    (* ListIndexZero should not occur here *)
    | ListIndexZero _ -> failwith "eval_str: incorrect List index"

(* Special functions evaluation *)
(* Each function has type basic_expr list -> entity *)
and apply_special_func (p : parameters) (fname : string) (el : basic_expr list) : entity =
  match fname, el with
    | "SEQ", [e; Arithm [Entity (Variable (Var vi))]; Complex z1; Complex z2; Complex z3] ->
      if z1.im = 0. && z2.im = 0. && z3.im = 0. then
        let coef = (z2.re -. z1.re)/.z3.re in
        let n = 1 + true_int_of_float coef in
        if coef < 0. then
          failwith "apply_special_func: wrong input for Seq"
        else
          let asc = z2.re > z1.re in
          let t = Array.make n QMark in
          (set_var p.var vi (complex_of_float z1.re);
          let i = ref 0 in
          while
            (if asc
              then access_real_var p.var vi <= z2.re
              else access_real_var p.var vi >= z2.re) do
            t.(!i) <- Complex (eval_num p e);
            set_real_var p.var vi ((access_real_var p.var vi) +. z3.re);
            incr i
          done;
          ListContent t)
      else failwith "apply_special_func: wrong input for Seq"

    | "PXLTEST", [Complex z1; Complex z2] ->
      if is_int z1 && is_int z2
        && z1.re >= 1. && z1.re <= 63.
        && z2.re >= 1. && z2.re <= 127. then
        let n1 = true_int_of_float z1.re in
        let n2 = true_int_of_float z2.re in
        Value
          (complex_of_bool
            (p.gscreen.(n1).(n2) || p.bgscreen.(n1).(n2)))
      else failwith "apply_special_func: wrong input for PxlTest"

    | "MATTOLIST", [Arithm [Entity (VarMat mi)]; Complex z]
    | "MATTOLIST", [Arithm [Entity (Variable (Var mi))]; Complex z] ->
      let j = int_of_complex z -1 in
      let m = p.mat.(if mi = 28 then 26 else mi) in (* Mat Ans is stored at position 26 *)
      let row = (Array.length m)/2 in
      ListContent (Array.init row (fun i -> Complex (get_complex m.(i).(j) m.(i+row).(j))))

    | "SIGMAPAR", _ ->
      (let arg_seq =
        if List.length el = 5
          then el
          else (* Case of omitted "Step" parameter: 1 by default *)
            (* Omitting the "Step" is allowed for Sigmapar but not Seq (weird) *)
            List.rev (Complex {re = 1.; im = 0.}::(List.rev el))
      in
      match apply_special_func p "SEQ" arg_seq with
        | ListContent t ->
          let sum_list = Arithm [Function ("SUM", [Arithm [Entity (ListContent t)]])] in
          let z = eval_num p sum_list in
          Value z
        | _ ->
          failwith "apply_special_func: wrong output of function Seq in
            internal computation of function Sigma")

    | "MATTOLIST", _ -> failwith "apply_special_func: wrong input for MatToList"
    | "PXLTEST", _ -> failwith "apply_special_func: wrong input for PxlTest"
    | "SEQ", _ -> failwith "apply_special_func: wrong input for Seq"
    | _ -> failwith ("apply_special_func: unknown function "^fname)


(* Final evaluation of the arithmetic formula *)
(* p is the parameter container, containing the variables, lists and matrices *)
and calculate (p : parameters) (outq : entity list) (opq : arithm list) : entity =
  match outq, opq with
    (* Right parentheses may be omitted at the end of expressions in Casio Basic *)
    | _, Lpar::opqt -> calculate p outq opqt
    | x2::x1::t, (Op o)::opqt -> calculate p ((apply_op p o x1 x2)::t) opqt
    | x::t, (Lunop (lo, a))::opqt -> calculate p ((apply_lop p lo x a)::t) opqt
    | [x], [] ->
      (match x with
        (* Numbers *)
        | Value _ -> x
        | Variable _ -> Value (get_val_numexpr p x)
        (* Lists and matrices *)
        | ListContent _
        | VarList _ -> ListContent (get_val_listexpr p x)
        | _ -> MatContent (get_val_matexpr p x))
    | x2::x1::t, [] -> calculate p ((apply_op p "TIMES" x1 x2)::t) []
    | _, (Op o)::opqt ->
      if o = "PLUS" then
        (* x + 0 = x *)
        calculate p outq opqt
      else failwith ("calculate: Not enough operands for operator "^o)
    | _ -> failwith "calculate: Syntax error"

(* Calculates the result of a sequence of right-associative operations
  (ex: 1+2^2^2 is reduced to 1+16) *)
and right_reduce (p : parameters) (output_q : entity list) (op_q : arithm list) : entity list * arithm list =
  match (output_q, op_q) with
    | _, []
    | _, Lpar::_ -> (output_q, op_q)
    | x2::x1::outq, (Op o)::opqt ->
      if left_assoc o
        then (output_q, op_q)
        else right_reduce p ((apply_op p o x1 x2)::outq) opqt
    | _, (Op o)::opqt ->
      if o = "PLUS" then
        (* x + 0 = x *)
        right_reduce p output_q opqt
      else failwith ("reduce: Not enough operands for operator "^o)
    | x::outq, (Lunop (lo, a))::opqt ->
        right_reduce p ((apply_lop p lo x a)::outq) opqt
    | _ -> failwith "reduce: Syntax error"

(* Shunting_yard algorithm: returns the value of the expression *)
(* p is the parameter container, containing the variables, lists and matrices *)
and shunting_yard (p : parameters) (alist : arithm list) (output_q : entity list) (op_q : arithm list) : entity =
  match (alist, op_q) with
    (* End case *)
    | [], _ -> calculate p output_q op_q

    (* Add to a queue *)
    (* Case of omitted multiplication operator *)
    | Rpar :: (Entity _) :: _, _
    (* | Rpar :: Lpar :: _, _ *) (* This case is handled during lexing *)
    | Rpar :: (Lunop _) :: _, _
    | Rpar :: (Function _) :: _, _ -> shunting_yard p (Rpar::(Op "OMITTEDTIMES")::List.tl alist) output_q op_q
    | (Entity x1)::(Entity _)::_, _
    | (Entity x1)::(Lunop _)::_, _
    | (Entity x1)::(Function _)::_, _ -> shunting_yard p ((Op "OMITTEDTIMES")::List.tl alist) (x1::output_q) op_q

    (* Weak omitted TIMES: has a slightly lesser precedence than OMITTEDTIMES (used to treat the "GH case", see tests/tests_arithmetic.ml) *)
    | (Entity x) :: Lpar :: _, _ -> shunting_yard p ((Op "WEAKOMTIMES")::List.tl alist) (x::output_q) op_q
    
      (* Normal number case *)
    | (Entity x)::t, _ -> shunting_yard p t (x::output_q) op_q
    (* Lpar *)
    | Lpar::t, _ -> shunting_yard p t output_q (Lpar::op_q)
    (* Lunop *)
    | (Lunop loa)::t, _ -> shunting_yard p t output_q ((Lunop loa)::op_q)

    (* Function evaluation *)
    | (Function (fname, el))::t, _ ->
      (* Complex functions (complex list -> complex) *)
      (if Hashtbl.mem func_table fname then
        let vl = List.map (fun e -> eval_num p e) el in
        shunting_yard p t ((Value (apply_func fname vl))::output_q) op_q
      (* Entity functions (entity list -> entity) *)
      else if Hashtbl.mem entity_func_table fname then
        let nl = List.map (fun e -> eval_entity p e) el in
        shunting_yard p t ((apply_entity_func fname nl)::output_q) op_q
      (* String functions, numerical output only (str_expr -> complex) *)
      else if List.mem fname numerical_string_functions then
        let sel =
          List.map
            (fun e ->
              match e with
                | StringExpr se -> se
                | _ -> failwith "Arithmetic parsing: wrong type in string function arguments")
            el
        in
        (match eval_str p (Str_Func (fname, sel)) with
          | Num_expr (Complex z) -> shunting_yard p t ((Value z)::output_q) op_q
          | _ -> failwith "Arithmetic parsing: wrong output type of string function")
      (* Special function: num_expr list -> entity *)
      (* Only the arguments for which the corresponding entry of eval_arg
        is true are evaluated before being passed to the function *)
      else if List.mem_assoc fname special_functions_list then
        let eval_arg = List.assoc fname special_functions_list in
        let n1 = List.length eval_arg in
        let n2 = List.length el in
        if n2 < n1 then
          failwith ("Arithmetic parsing: wrong arity for special function "^fname)
        else
          (let eval_arg_full =
            if n2 > n1
              then
                (* Case of more arguments than specified:
                  they are accepted as they may be optional parameters *)
                (* In this case, complete with some true *)
                List.rev
                  (List.rev_append
                    (Array.to_list (Array.make (n2-n1) true))
                    (List.rev eval_arg))
              else eval_arg
          in 
          let eval_el =
            List.rev (List.rev_map2
              (fun do_eval e ->
                if do_eval
                  then Complex (eval_num p e)
                  else e)
              eval_arg_full el)
          in
          shunting_yard p t ((apply_special_func p fname eval_el)::output_q) op_q)
      else
        failwith ("Arithmetic parsing: Unknown function "^fname))

    (* Runop *)
    | (Runop ro)::t, _ ->
      (match output_q with
        | x::outq -> shunting_yard p ((Entity (apply_rop p ro x))::t) outq op_q
        | _ -> failwith ("Arithmetic parsing: No operand for operator "^ro))

    (* Op *)
    | (Op o1)::t, (Op o2)::opqt ->
      if precedence o2 o1 = 1 || (precedence o2 o1 = 0 && left_assoc o1)
        then if left_assoc o2
          then
            (match output_q with
              | x2::x1::outq ->
                shunting_yard p alist ((apply_op p o2 x1 x2)::outq) opqt
              | _ ->
                (* Annoying behavior of PLUS operator *)
                if o1 = "PLUS" then
                  (* Skip the excess PLUS *)
                  shunting_yard p t output_q op_q
                else failwith ("Arithmetic parsing: Not enough operands for operator "^o2))
          else
            let noutq, nopq = right_reduce p output_q op_q in
            shunting_yard p alist noutq nopq
        else shunting_yard p t output_q ((Op o1)::op_q)
    | (Op o)::t, (Lunop _)::opqt ->
      (* If the operator is POWER, NSQRT or OMITTEDTIMES, the operator should be applied first,
         otherwise the Lunop is applied first *)
      if precedence o "OMITTEDTIMES" >= 0
        then
          shunting_yard p t output_q ((Op o)::op_q)
        else
          let noutq, nopq = right_reduce p output_q op_q in
          shunting_yard p alist noutq nopq
    | (Op o)::t, _ -> shunting_yard p t output_q ((Op o)::op_q)
    
    (* Rpar *)
    | Rpar::t, _::_ ->
      (match output_q, op_q with
        | _, Lpar::opqt -> shunting_yard p t output_q opqt

        (* Operator evaluation *)
        | x::outq, (Lunop (lo, a))::opqt ->
          shunting_yard p (Rpar::t) ((apply_lop p lo x a)::outq) opqt
        | x2::x1::outq, (Op o)::opqt ->
          shunting_yard p (Rpar::t) ((apply_op p o x1 x2)::outq) opqt
        | _, (Op o)::opqt -> failwith ("Arithmetic parsing: Not enough operands for operator "^o)

        (* Errors *)
        | _, [] -> failwith "Arithmetic parsing: Mismatched parentheses"
        
        | _ -> failwith "Arithmetic parsing: Untreated case")
    
    (* String functions *)
    (* to do *)
    | _,_ -> failwith "Arithmetic parsing: Syntax error"

(* General evaluation function, returns an entity *)
(* p is the parameter container, containing the variables, lists and matrices *)
and eval_entity (p : parameters) (e : basic_expr) : entity =
  match e with
    | Complex z -> Value z
    | Arithm al -> shunting_yard p al [] []
    | StringExpr se ->
      (match eval_str p se with
        | Num_expr (Complex z) -> Value z
        | _ -> failwith "eval_entity: error, string expression evaluation returned a non-numerical string value")
    | _ -> failwith "eval_entity: error, QMark provided"

(* Numerical evaluation function *)
and eval_num (p : parameters) (e : num_expr) : complex =
  match e with
    | Complex z -> z
    | Arithm al ->
      (match shunting_yard p al [] [] with
        | Value z -> z
        | _ -> failwith "eval_num: error, wrong output type")
    | StringExpr se ->
      (match eval_str p se with
        | Num_expr (Complex z) -> z
        | _ -> failwith "eval_num: error, string expression evaluation returned a non-numerical string value")
    | _ -> failwith "eval_num: error, QMark provided";;

(* List expression evaluation function *)
let eval_list (p : parameters) (e : list_expr) : float array =
  match e with
    | Arithm al ->
      (match shunting_yard p al [] [] with
        | ListContent t -> numexpr_to_float_array t
        | _ -> failwith "eval_list: error, wrong output type")
    | _ -> failwith "eval_list: error, wrong input type";;

(* List expression evaluation function *)
let eval_mat (p : parameters) (e : mat_expr) : float array array =
  match e with
    | Arithm al ->
      (match shunting_yard p al [] [] with
        | MatContent m -> numexpr_to_float_matrix m
        | _ -> failwith "eval_mat: error, wrong output type")
    | _ -> failwith "eval_mat: error, wrong input type";;