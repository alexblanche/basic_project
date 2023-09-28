(* Unit tests for the arithmetic functions *)

exception Test_failed of int

(* Tests for lexing *)
let unit_tests_lexer () =
  let check i (slist, expected) =
    (* print_string ("Test "^(string_of_int i)^"... "); *)
    match extract_expr slist with
      | (Arithm alist), _, _ ->
        if alist <> expected
          then raise (Test_failed i)
          (* else print_endline "Done." *)
      | Complex z, _, _ -> raise (Test_failed i)
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
      [Lunop "ABS"; Lpar; Lpar; Entity (Value {re = 3.; im = 0.}); Rpar; Rpar; Runop "EXCLAMATIONMARK"]);
    (["2"; "POWER"; "2"; "POWER"; "2"; "POWER"; "2"],
      [Entity (Value {re = 2.; im = 0.}); Op "POWER"; Entity (Value {re = 2.; im = 0.});
      Op "POWER"; Entity (Value {re = 2.; im = 0.}); Op "POWER"; Entity (Value {re = 2.; im = 0.})]);
    (["ABS"; "ABS"; "ABS"; "5"; "."; "EXCLAMATIONMARK"],
      [Lunop "ABS"; Lunop "ABS"; Lunop "ABS"; Entity (Value {re = 5.; im = 0.}); Runop "EXCLAMATIONMARK"]);
    (["LPAR"; "ABS"; "5"; "."; "RPAR"], [Lpar; Lunop "ABS"; Entity (Value {re = 5.; im = 0.}); Rpar]);
    (["1"; "PLUS"; "2"; "TIMES"; "CPLXI"],
      [Entity (Value {re = 1.; im = 0.}); Op "PLUS"; Entity (Value {re = 2.; im = 0.}); Op "TIMES";
      Entity (Value {re = 0.; im = 1.})]);
    (["2"; "TIMES"; "B"; "PLUS"; "A"],
      [Entity (Value {re = 2.; im = 0.}); Op "TIMES"; Entity (Variable (Var 1)); Op "PLUS"; Entity (Variable (Var 0))]);
    (["2"; "B"; "PLUS"; "3"; "A"; "MINUS"; "4"; "CPLXI"],
      [Entity (Value {re = 2.; im = 0.}); Entity (Variable (Var 1)); Op "PLUS"; Entity (Value {re = 3.; im = 0.});
      Entity (Variable (Var 0)); Op "MINUS"; Entity (Value {re = 4.; im = 0.}); Entity (Value {re = 0.; im = 1.})]);
    (["1"; "PLUS"; "ABS"; "LPAR"; "UMINUS"; "8"; "RPAR"; "ASSIGN"; "A"],
      [Entity (Value {re = 1.; im = 0.}); Op "PLUS"; Lunop "ABS"; Lpar; Lunop "UMINUS"; Entity (Value {re = 8.; im = 0.}); Rpar]);
    (["1"; "PLUS"; "MAX"; "4"; "."; "5"; ","; "LPAR"; "7"; "TIMES"; "1"; "RPAR"; ","; "3"; "TIMES"; "2"; "RPAR";
      "TIMES"; "2"; "EOL"],
    [Entity (Value {re = 1.; im = 0.}); Op "PLUS";
      Function ("MAX",
        [
          Complex {re = 4.5; im = 0.};
          Arithm [Lpar; Entity (Value {re = 7.; im = 0.}); Op "TIMES"; Entity (Value {re = 1.; im = 0.}); Rpar];
          Arithm [Entity (Value {re = 3.; im = 0.}); Op "TIMES"; Entity (Value {re = 2.; im = 0.})]
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
        "LSQBRACKET"; "4"; "PLUS"; "2"; ","; "A"; "RBRACKET";
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
      ])
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

  let check i (slist, expected) =
    let (expr, _, _) = extract_expr slist in
    let z = eval_num p expr in
    if is_not_zero (Complex.sub z expected)
      then raise (Test_failed i)
      (* else print_endline ("Test "^(string_of_int i)^" passed") *)
  in
  try
    List.iteri check [
      (["1"], {re = 1.; im = 0.});
      (["1"; "."; "5"; "PLUS"; "2"; "."; "5"], {re = 4.; im = 0.});
      (["1"; "."; "5"; "PLUS"; "LPAR"; "2"; "."; "5"; "TIMES"; "3"; "."; "5"; "RPAR";
        "POWER"; "LPAR"; "5"; "MINUS"; "3"; "RPAR"],
        {re = 78.0625; im = 0.});
      (["ABS"; "LPAR"; "LPAR"; "3"; "."; "RPAR"; "RPAR"; "EXCLAMATIONMARK"], {re = 6.; im = 0.});
      (["2"; "POWER"; "2"; "POWER"; "2"; "POWER"; "2"], {re = 256.; im = 0.});
      (["ABS"; "ABS"; "ABS"; "5"; "."; "EXCLAMATIONMARK"], {re = 120.; im = 0.});
      (["LPAR"; "ABS"; "5"; "."; "RPAR"], {re = 5.; im = 0.});
      (["1"; "PLUS"; "2"; "TIMES"; "CPLXI"], {re = 1.; im = 2.});
      (["2"; "TIMES"; "B"; "PLUS"; "A"], {re = 16.; im = 8.});
      (["2"; "B"; "PLUS"; "3"; "A"; "MINUS"; "4"; "CPLXI"], {re = 36.; im = 4.});
      (["1"; "PLUS"; "ABS"; "LPAR"; "UMINUS"; "8"; "RPAR"; "ASSIGN"; "A"], {re = 9.; im = 0.});
      (["1"; "PLUS"; "MAX"; "4"; "."; "5"; ","; "LPAR"; "7"; "TIMES"; "1"; "RPAR"; ","; "3"; "TIMES"; "2"; "RPAR";
      "TIMES"; "2"; "EOL"], {re = 15.; im = 0.})
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

  let check i (slist, expected) =
    let (expr, expr_type, _) = extract_expr slist in
    match expr with
      | Arithm al ->
        let equal =
          (match shunting_yard p al [] [], expr_type, expected with
            | ListContent t, ListExpr, ListContent tex -> t = tex
            | MatContent m, MatExpr, MatContent mex -> m = mex
            | _ -> raise (Test_failed i))
        in
        if not equal
          then raise (Test_failed i)
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
        "LSQBRACKET"; "4"; "PLUS"; "2"; ","; "9"; "0"; "RBRACKET";
      "RSQBRACKET";
      "PLUS"; "1"],
        MatContent
        [|[|Complex {re = 4.; im = 0.}; Complex {re = 5.; im = 0.}|];
          [|Complex {re = 0.; im = 0.}; Complex {re = -84.; im = 0.}|]|])
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
