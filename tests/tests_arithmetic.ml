(* Unit tests for arithmetic_parsing.ml *)

#use "basic_parsing/arithmetic_parsing.ml"

exception Test_failed of int

let unit_tests_lexer () =
  let check i (expr, lexlist) =
    if (lexer expr) <> lexlist
      then raise (Test_failed i)
  in
  try
    List.iteri check [
    ("1.", [Float 1.]);
    ("1.5+2.5", [Float 1.5; Op "+"; Float 2.5]);
    ("1.5+(2.5*3.5)^(5.-3.)",
      [Float 1.5; Op "+"; Lpar; Float 2.5 ; Op "*";
      Float 3.5; Rpar; Op "^"; Lpar; Float 5.;
      Op "-"; Float 3.; Rpar]);
    ("Abs((3.))!", [Lunop "Abs"; Lpar; Lpar; Float 3.; Rpar; Rpar; Runop "!"]);
    ("2.^2.^2.^2.", [Float 2.; Op "^"; Float 2.; Op "^"; Float 2.; Op "^"; Float 2.]);
    ("Abs Abs Abs    5. !", [Lunop "Abs"; Lunop "Abs"; Lunop "Abs"; Float 5.; Runop "!"]);
    ("(Abs 5.)", [Lpar; Lunop "Abs"; Float 5.; Rpar])
    ];
    print_endline "Tests_arithmetic_parsing, lexer: all tests ran successfully"
  with
    | Test_failed i ->
      print_endline ("Tests_arithmetic_parsing, lexer: test "^(string_of_int i)^" failed")
    | Failure s ->
      print_endline ("Tests_arithmetic_parsing, lexer: one test encountered an error \""^s^"\"");;

let unit_tests_eval () =
  let check i (expr, res) =
    if (eval expr) <> res
      then raise (Test_failed i)
      (* else print_endline ("Test "^(string_of_int i)^" passed") *)
  in
  try
    List.iteri check [
    ("1.", 1.);
    ("1.5+2.5", 4.);
    ("1.5+(2.5*3.)^(5.-3.)", 57.75);
    ("Abs((3.))!", 6.);
    ("2.^2.^2.^2.", 65536.);
    ("Abs Abs Abs    5. !", 120.);
    ("(Abs 5.)", 5.)
    ];
    print_endline "Tests_arithmetic_parsing, eval: all tests ran successfully"
  with
    | Test_failed i ->
      print_endline ("Tests_arithmetic_parsing, eval: test "^(string_of_int i)^" failed")
    | Failure s ->
      print_endline ("Tests_arithmetic_parsing, eval: one test encountered an error \""^s^"\"");;



unit_tests_lexer ();;
unit_tests_eval ();;