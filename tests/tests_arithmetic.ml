(* Unit tests for the arithmetic functions *)

exception Test_failed of int

(* Tests for lexing *)
let unit_tests_lexer () =
  let check i (slist, expected) =
    (* print_string ("Test "^(string_of_int i)^"... "); *)
    match extract_expr slist with
      | (Arithm alist), _ ->
        if alist <> expected then
          (print_endline (String.concat " " slist);
          raise (Test_failed i))
          (* else print_endline "Done." *)
      | Complex z, _ -> raise (Test_failed i)
      | _ -> failwith ("Test "^(string_of_int i)^": QMark is not supported for these tests")
  in
  try
    List.iteri check [
    (["1"; "."; "5"; "PLUS"; "2"; "."; "5"],
      [Entity (Value {re = 1.5; im = 0.}); Op "PLUS"; Entity (Value {re = 2.5; im = 0.})]);
    (["1"; "."; "5"; "PLUS"; "LPAR"; "2"; "."; "5"; "TIMES"; "3"; "."; "5"; "RPAR"; "POWER"; "LPAR"; "5"; "MINUS"; "3"; "RPAR"],
      [Entity (Value {re = 1.5; im = 0.}); Op "PLUS"; Lpar; Entity (Value {re = 2.5; im = 0.}) ; Op "TIMES";
      Entity (Value {re = 3.5; im = 0.}); Rpar; Op "POWER"; Lpar; Entity (Value {re = 5.; im = 0.});
      Op "MINUS"; Entity (Value {re = 3.; im = 0.}); Rpar]);
    (["ABS"; "LPAR"; "LPAR"; "3"; "."; "RPAR"; "RPAR"; "EXCLAMATIONMARK"],
      [Lunop ("ABS", true); Lpar; Lpar; Entity (Value {re = 3.; im = 0.}); Rpar; Rpar; Runop "EXCLAMATIONMARK"]);
    (["2"; "POWER"; "2"; "POWER"; "2"; "POWER"; "2"],
      [Entity (Value {re = 2.; im = 0.}); Op "POWER"; Entity (Value {re = 2.; im = 0.});
      Op "POWER"; Entity (Value {re = 2.; im = 0.}); Op "POWER"; Entity (Value {re = 2.; im = 0.})]);
    (["ABS"; "ABS"; "ABS"; "5"; "."; "EXCLAMATIONMARK"],
      [Lunop ("ABS", true); Lunop ("ABS", true); Lunop ("ABS", true); Entity (Value {re = 5.; im = 0.}); Runop "EXCLAMATIONMARK"]);
    (["LPAR"; "ABS"; "5"; "."; "RPAR"], [Lpar; Lunop ("ABS", true); Entity (Value {re = 5.; im = 0.}); Rpar]);
    (["1"; "PLUS"; "2"; "TIMES"; "CPLXI"],
      [Entity (Value {re = 1.; im = 0.}); Op "PLUS"; Entity (Value {re = 2.; im = 0.}); Op "TIMES";
      Entity (Value {re = 0.; im = 1.})]);
    (["2"; "TIMES"; "B"; "PLUS"; "A"],
      [Entity (Value {re = 2.; im = 0.}); Op "TIMES"; Entity (Variable (Var 1)); Op "PLUS"; Entity (Variable (Var 0))]);
    (["2"; "B"; "PLUS"; "3"; "A"; "MINUS"; "4"; "CPLXI"],
      [Entity (Value {re = 2.; im = 0.}); Entity (Variable (Var 1)); Op "PLUS"; Entity (Value {re = 3.; im = 0.});
      Entity (Variable (Var 0)); Op "MINUS"; Entity (Value {re = 4.; im = 0.}); Entity (Value {re = 0.; im = 1.})]);
    (["1"; "PLUS"; "ABS"; "LPAR"; "UMINUS"; "8"; "RPAR"; "ASSIGN"; "A"],
      [Entity (Value {re = 1.; im = 0.}); Op "PLUS"; Lunop ("ABS", true); Lpar; Lunop ("UMINUS", true); Entity (Value {re = 8.; im = 0.}); Rpar]);
    (["1"; "PLUS"; "MAX"; "LBRACKET"; "4"; "."; "5"; ","; "LPAR"; "7"; "TIMES"; "1"; "RPAR"; ","; "3"; "TIMES"; "2"; "RBRACKET"; "RPAR";
      "TIMES"; "2"; "EOL"],
    [Entity (Value {re = 1.; im = 0.}); Op "PLUS";
      Function ("MAX",
        [Arithm
          [Entity
            (ListContent
              [|
                Complex {re = 4.5; im = 0.};
                Arithm [Lpar; Entity (Value {re = 7.; im = 0.}); Op "TIMES"; Entity (Value {re = 1.; im = 0.}); Rpar];
                Arithm [Entity (Value {re = 3.; im = 0.}); Op "TIMES"; Entity (Value {re = 2.; im = 0.}) ]
              |]
            )
          ]
        ]);
      Op "TIMES"; Entity (Value {re = 2.; im = 0.})
    ]);

    (* List/Mat arithmetic *)
    (["2"; "TIMES"; "B"; "PLUS"; "LBRACKET"; "8"; ","; "8"; "PLUS"; "2"; ","; "A"; "RBRACKET"],
      [Entity (Value {re = 2.; im = 0.}); Op "TIMES"; Entity (Variable (Var 1)); Op "PLUS";
      Entity
        (ListContent
          [|Complex {re = 8.; im = 0.};
            Arithm [Entity (Value {re = 8.; im = 0.}); Op "PLUS"; Entity (Value {re = 2.; im = 0.})];
            Arithm [Entity (Variable (Var 0))]
            |]
        )
      ]);

    (["5"; "MINUS";
      "LSQBRACKET";
        "LSQBRACKET"; "2"; ","; "1"; "RSQBRACKET";
        "LSQBRACKET"; "4"; "PLUS"; "2"; ","; "A"; "RSQBRACKET";
      "RSQBRACKET";
      "PLUS"; "0"],
      [Entity (Value {re = 5.; im = 0.}); Op "MINUS";
        Entity
          (MatContent
            [| [| Complex {re = 2.; im = 0.}; Complex {re = 1.; im = 0.} |];
              [| Arithm [Entity (Value {re = 4.; im = 0.}); Op "PLUS"; Entity (Value {re = 2.; im = 0.})];
                  Arithm [Entity (Variable (Var 0))] |] |]
          );
        Op "PLUS";
        Entity (Value {re = 0.; im = 0.})
      ]);

    (["STRLEN"; "STR"; "1"; "RPAR"; "MINUS"; "1"],
      [Function ("STRLEN", [StringExpr (Str_access 0)]); Op "MINUS";
      Entity (Value {Complex.re = 1.; im = 0.})]);

    (["LBRACKET"; "1"; "TIMES"; "2"],
      [Entity
        (ListContent
          [|Arithm
            [Entity (Value {Complex.re = 1.; im = 0.});
            Op "TIMES";
            Entity (Value {Complex.re = 2.; im = 0.})]
          |]
        )
      ]);

    (["DIM"; "LIST"; "2"; "5"; "EQUAL"; "1"],
      [Lunop ("DIM", false); Entity (VarList (Value {Complex.re = 25.; im = 0.}));
        Op "EQUAL"; Entity (Value {Complex.re = 1.; im = 0.})]);
    ];
    print_endline "-----------------------------------------";
    print_endline "Tests_arithmetic, lexer: all tests passed";
    print_endline "-----------------------------------------"
  with
    | Test_failed i ->
      print_endline ("Tests_arithmetic, lexer: test "^(string_of_int i)^" failed")
    | Failure s ->
      print_endline ("Tests_arithmetic, lexer: one test encountered an error \""^s^"\"");;


(* Tests for numerical evaluation *)
let unit_tests_eval_num () =
  let p = empty_param () in
  (* 10 -> A *)
  p.var.(0) <- 10.;
  (* 3+4i -> B *)
  p.var.(1) <- 3.;
  p.var.(1+29) <- 4.;
  (* List 1 *)
  p.list.(0) <- [| 5.; 2.; 3.; 1. |];

  let check i (slist, expected) =
    let (expr, _) = extract_expr slist in
    let z = eval_num p expr in
    if not (are_equal z expected) then
      (print_endline (String.concat " " slist);
      raise (Test_failed i))
      (* else print_endline ("Test "^(string_of_int i)^" passed") *)
  in
  try
    List.iteri check [
      (["1"], {re = 1.; im = 0.});
      (["1"; "."; "5"; "PLUS"; "2"; "."; "5"], {re = 4.; im = 0.});
      (["1"; "."; "5"; "PLUS"; "LPAR"; "2"; "."; "5"; "TIMES"; "3"; "."; "5"; "RPAR";
        "POWER"; "LPAR"; "5"; "MINUS"; "3"; "RPAR"], {re = 78.0625; im = 0.});
      (["ABS"; "LPAR"; "LPAR"; "3"; "."; "RPAR"; "RPAR"; "EXCLAMATIONMARK"], {re = 6.; im = 0.});
      (["2"; "POWER"; "2"; "POWER"; "2"; "POWER"; "2"], {re = 256.; im = 0.});
      (["ABS"; "ABS"; "ABS"; "5"; "."; "EXCLAMATIONMARK"], {re = 120.; im = 0.});
      (["LPAR"; "ABS"; "5"; "."; "RPAR"], {re = 5.; im = 0.});
      (["1"; "PLUS"; "2"; "TIMES"; "CPLXI"], {re = 1.; im = 2.});
      (["2"; "TIMES"; "B"; "PLUS"; "A"], {re = 16.; im = 8.});
      (["2"; "B"; "PLUS"; "3"; "A"; "MINUS"; "4"; "CPLXI"], {re = 36.; im = 4.});
      (["1"; "PLUS"; "ABS"; "LPAR"; "UMINUS"; "8"; "RPAR"; "ASSIGN"; "A"], {re = 9.; im = 0.});
      (["1"; "PLUS"; "MAX"; "LBRACKET"; "4"; "."; "5"; ","; "LPAR"; "7"; "TIMES"; "1"; "RPAR"; ","; "3"; "TIMES"; "2"; "RPAR"; "RBRACKET";
      "TIMES"; "2"; "EOL"], {re = 15.; im = 0.});
      (["LIST"; "1"; "LSQBRACKET"; "2"; "MINUS"; "1"; "RSQBRACKET"], {re = 5.; im = 3.});
      (["2"; "EQUAL"; "REP"; "2"; "AND"; "1"], {re = 1.; im = 0.});
      (["MINUS"; "1"], {re = -1.; im = 0.});
      (["2"; "TIMES"; "UMINUS"; "1"], {re = -2.; im = 0.});
      (["2"; "LPAR"; "1"; "PLUS"; "1"], {re = 4.; im = 0.});
      (["LPAR"; "1"; "PLUS"; "1"; "RPAR"; "MINUS"; "1"], {re = 1.; im = 0.});
      (["LPAR"; "MINUS"; "1"; "PLUS"; "1"; "RPAR"; "MINUS"; "1"], {re = -1.; im = 0.});
      (["1"; "MINUS"; "1"; "PLUS"; "MINUS"; "1"], {re = -1.; im = 0.});
      (["4"; "DIFFERENT"; "4"; "MINUS"; "1"; "AND"; "0"], {re = 0.; im = 0.});
      (["0"; "EQUAL"; "0"; "PLUS"; "1"; "AND"; "0"], {re = 0.; im = 0.});
      (["2"; "TIMES"; "2"; "POWER"; "3"; "PLUS"; "1"], {re = 17.; im = 0.});
      (["LPAR"; "2"; "RPAR"; "LPAR"; "3"; "RPAR"], {re = 6.; im = 0.});
      (["2"; "POWER"; "3"; "ABS"; "LPAR"; "UMINUS"; "8"; "RPAR"], {re = 64.; im = 0.});
      (["2"; "POWER"; "LPAR"; "3"; "RPAR"; "TIMES"; "ABS"; "LPAR"; "UMINUS"; "8"; "RPAR"], {re = 64.; im = 0.});
      (["2"; "POWER"; "LPAR"; "3"; "RPAR"; "ABS"; "LPAR"; "UMINUS"; "8"; "RPAR"], {re = 64.; im = 0.});



      (* Tests for precedence *)
      (* All these expressions distinguish operators of different precedence *)
      (* For every A op1 B op2 C, it was checked that (A op1 B) op2 C
        has a different value than A op1 (B op2 C),
        and the conclusion is stated in the comment on the right *)
      (["2"; "PLUS"; "2"; "TIMES"; "3"], {re = 8.; im = 0.}); (* TIMES <= PLUS *)
      (["2"; "OR"; "2"; "INTDIV"; "3"], {re = 1.; im = 0.}); (* INTDIV <= OR *)
      (["2"; "PLUS"; "2"; "INTDIV"; "3"], {re = 2.; im = 0.}); (* INTDIV <= PLUS *)
      (["2"; "INTDIV"; "1"; "TIMES"; "2"], {re = 4.; im = 0.}); (* INTDIV <= TIMES *)
      (["1"; "2"; "INTDIV"; "3"; "POWER"; "2"], {re = 1.; im = 0.}); (* POWER <= INTDIV *)
      (["1"; "2"; "RMDR"; "3"; "POWER"; "2"], {re = 3.; im = 0.}); (* POWER <= RMDR *)
      (["8"; "AND"; "9"; "INTDIV"; "3"], {re = 1.; im = 0.}); (* INTDIV <= AND *)
      (["2"; "PLUS"; "2"; "OR"; "3"], {re = 1.; im = 0.}); (* PLUS <= OR *)
      (["2"; "PLUS"; "2"; "AND"; "3"], {re = 1.; im = 0.}); (* PLUS <= AND *)
      (["2"; "PLUS"; "6"; "DIVIDED"; "3"], {re = 4.; im = 0.}); (* PLUS <= DIVIDED *)
      (["2"; "PLUS"; "6"; "POWER"; "3"], {re = 218.; im = 0.}); (* POWER <= PLUS *)
      (["2"; "TIMES"; "6"; "POWER"; "3"], {re = 432.; im = 0.}); (* POWER <= TIMES *)
      (["2"; "INTDIV"; "5"; "RMDR"; "3"], {re = 0.; im = 0.}); (* INTDIV = RMDR *)
      (["1"; "2"; "RMDR"; "7"; "INTDIV"; "3"], {re = 1.; im = 0.});
      (["2"; "DIVIDED"; "6"; "TIMES"; "3"], {re = 1.; im = 0.}); (* DIVIDED = TIMES *)
      (["2"; "MINUS"; "1"; "PLUS"; "3"], {re = 4.; im = 0.}); (* MINUS is left-associative *)
      (["2"; "PLUS"; "1"; "LEQ"; "3"], {re = 1.; im = 0.}); (* PLUS <= LEQ *)
      (["2"; "PLUS"; "1"; "EQUAL"; "3"], {re = 1.; im = 0.}); (* PLUS <= EQUAL *)
      (["2"; "RMDR"; "3"; "TIMES"; "2"], {re = 4.; im = 0.}); (* RMDR <= TIMES *)
      (["1"; "LESS"; "3"; "GREATER"; "0"], {re = 1.; im = 0.}); (* LESS = GREATER *)
      (["1"; "GREATER"; "3"; "LESS"; "0"], {re = 0.; im = 0.});
      (["1"; "EQUAL"; "3"; "GREATER"; "0"], {re = 0.; im = 0.}); (* GREATER = EQUAL *)
      (["3"; "GREATER"; "3"; "EQUAL"; "8"], {re = 0.; im = 0.});
      (["1"; "OR"; "1"; "AND"; "0"], {re = 1.; im = 0.}); (* AND <= OR *)
      (["0"; "AND"; "0"; "OR"; "1"], {re = 1.; im = 0.}); (* AND <= OR *)
      (["1"; "AND"; "3"; "EQUAL"; "0"], {re = 0.; im = 0.}); (* EQUAL <= AND *)
      (["1"; "OR"; "3"; "EQUAL"; "0"], {re = 1.; im = 0.}); (* EQUAL <= OR *)
      (["1"; "XOR"; "0"; "OR"; "1"], {re = 1.; im = 0.}); (* OR = XOR *)
      (["1"; "OR"; "1"; "XOR"; "1"], {re = 0.; im = 0.});
      (["2"; "POWER"; "2"; "NSQRT"; "8"; "1"], {re = 3.; im = 0.}); (* POWER <= NSQRT *)
      (["2"; "TIMES"; "2"; "NSQRT"; "8"; "1"], {re = 18.; im = 0.}); (* NSQRT <= TIMES *)
      (["2"; "INTDIV"; "2"; "NSQRT"; "8"; "1"], {re = 0.; im = 0.}); (* NSQRT <= INTDIV *)
      (["2"; "RMDR"; "2"; "NSQRT"; "8"; "1"], {re = 2.; im = 0.}); (* NSQRT <= RMDR *)

      (* 
           POWER
        << NSQRT
        << (INTDIV, RMDR)
        << (TIMES, DIVIDED)
        << (PLUS, MINUS)
        << (LEQ, LESS, GEQ, GREATER, EQUAL, DIFFERENT)
        << AND
        << (OR, XOR)
     *)

    (* Tests for higher precedence of omitted multiplication *)
    (["INT"; "0"; "."; "5"; "A"], {re = 5.; im = 0.});
    (["1"; "PLUS"; "LPAR"; "4"; "RPAR"; "LPAR"; "0"; "RPAR"], {re = 1.; im = 0.});

    (* Accuracy of Frac *)
    (["FRAC"; "1"; "5"; "4"; "."; "6"; "7"; "8"], {re = 0.678; im = 0.});
    (["FRAC"; "5"; "4"; "."; "6"; "7"; "8"], {re = 0.678; im = 0.});
    (["FRAC"; "2"; "5"; "3"; "."; "1"; "4"; "2"; "7"; "9"], {re = 0.14279; im = 0.});
    (["FRAC"; "1"; "."; "2"; "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0"; "1"], {re = 0.20000000000001; im = 0.});
    (["FRAC"; "1"; "."; "2"; "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0"; "1"], {re = 0.2; im = 0.});

    (* Accuracy *)
    (["5"; "."; "1"; "2"; "3"; "7"; "TIMESTENPOWER"; "MINUS"; "2"; "0"; "TIMES"; "2"], {re = 1.02474e-19; im = 0.});
    (["0"; "."; "1"; "PLUS"; "0"; "."; "2"; "MINUS"; "0"; "."; "3"], {re = 0.; im = 0.});
    (["0"; "."; "1"; "PLUS"; "0"; "."; "2"; "EQUAL"; "0"; "."; "3"], {re = 1.; im = 0.});
    ];
    print_endline "--------------------------------------------";
    print_endline "Tests_arithmetic, eval_num: all tests passed";
    print_endline "--------------------------------------------"
  with
    | Test_failed i ->
      print_endline ("Tests_arithmetic, eval_num: test "^(string_of_int i)^" failed")
    | Failure s ->
      print_endline ("Tests_arithmetic, eval_num: one test encountered an error \""^s^"\"");;


(* Tests for list and matrix arithmetic *)
let unit_tests_eval_list_mat () =  
  let p = empty_param () in
  p.list.(0) <- [| 5.; 2.; 3.; 1. |];
  p.list.(1) <- [| 8.; 8.; 8.; 0.; 0.; 0. |];

  p.mat.(2) <- [|[|4.; 5.|]; [|6.; 8.|]; [|0.; 0.|]; [|0.; 0.|]|];

  let check i (slist, expected) =
    let (expr, _) = extract_expr slist in
    match expr with
      | Arithm al ->
        let equal =
          (match shunting_yard p al [] [], expected with
            | ListContent t, ListContent tex -> t = tex
            | MatContent m, MatContent mex -> m = mex
            | Value z, Value zex -> z = zex
            | _ -> false)
        in
        if not equal then
          (print_endline (String.concat " " slist);
          raise (Test_failed i))
          (* else print_endline ("Test "^(string_of_int i)^" passed") *)
      | _ -> failwith "Unexpected QMark or Complex"
    
  in
  try
    List.iteri check [
      (["2"; "TIMES"; "5"; "PLUS"; "LBRACKET"; "8"; ","; "8"; "PLUS"; "2"; ","; "1"; "0"; "0"; "RBRACKET"],
        ListContent [| Complex {re = 18.; im = 0.} ; Complex {re = 20.; im = 0.}; Complex {re = 110.; im = 0.}|]);

      (["5"; "MINUS";
        "LSQBRACKET";
          "LSQBRACKET"; "2"; ","; "1"; "RSQBRACKET";
          "LSQBRACKET"; "4"; "PLUS"; "2"; ","; "9"; "0"; "RSQBRACKET";
        "RSQBRACKET";
        "PLUS"; "1"],
        MatContent
          [|[|Complex {re = 4.; im = 0.}; Complex {re = 5.; im = 0.}|];
            [|Complex {re = 0.; im = 0.}; Complex {re = -84.; im = 0.}|]|]);

      (["LIST"; "1"],
        ListContent [| Complex {re = 5.; im = 3.} ; Complex {re = 2.; im = 1.}|]);

      (["LSQBRACKET";
          "LSQBRACKET"; "2"; ","; "1"; "RSQBRACKET";
          "LSQBRACKET"; "4"; ","; "5"; "RSQBRACKET";
        "RSQBRACKET";
        "PLUS";
        "MAT"; "C"],
        MatContent
          [|[|Complex {re = 6.; im = 0.}; Complex {re = 6.; im = 0.}|];
            [|Complex {re = 10.; im = 0.}; Complex {re = 13.; im = 0.}|]|]);

      (["LBRACKET"; "8"; ","; "2"; "EQUAL"; "2"],
        ListContent [|Complex {re = 8.; im = 0.}; Complex {re = 1.; im = 0.};|]);

      (["LBRACKET"; "8"; ","; "2"; "RBRACKET"; "EQUAL"; "2"],
        ListContent [|Complex {re = 0.; im = 0.}; Complex {re = 1.; im = 0.};|]);

      (["DIM"; "LIST"; "2"; "EQUAL"; "3"],
        Value {re = 1.; im = 0.});
    ];
    print_endline "-------------------------------------------------";
    print_endline "Tests_arithmetic, eval_list_mat: all tests passed";
    print_endline "-------------------------------------------------"
  with
    | Test_failed i ->
      print_endline ("Tests_arithmetic, eval_list_mat: test "^(string_of_int i)^" failed")
    | Failure s ->
      print_endline ("Tests_arithmetic, eval_list_mat: one test encountered an error \""^s^"\"");;



(* Execution of the tests *)
unit_tests_lexer ();;
unit_tests_eval_num ();;
unit_tests_eval_list_mat ();;
