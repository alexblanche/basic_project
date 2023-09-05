(* Unit tests for arithmetic_parsing.ml *)

(* #use "basic_parsing/arithmetic/complex.ml"
#use "basic_parsing/basic_type.ml"
#use "basic_parsing/project_type.ml"
#use "basic_parsing/arithmetic/arithmetic_def.ml"
#use "basic_parsing/arithmetic/arithmetic_lexing.ml"
#use "basic_parsing/arithmetic/arithmetic_parsing.ml" *)

exception Test_failed of int

let unit_tests_lexer () =
  let check i (slist, expected) =
    let (Arithm alist), _ = extract_expr slist in
    if alist <> expected
      then raise (Test_failed i)
  in
  try
    List.iteri check [
    (["1"], [Number (Value {re = 1.; im = 0.})]);
    (["1"; "."; "5"; "PLUS"; "2"; "."; "5"],
      [Number (Value {re = 1.5; im = 0.}); Op "PLUS"; Number (Value {re = 2.5; im = 0.})]);
    (["1"; "."; "5"; "PLUS"; "LPAR"; "2"; "."; "5"; "TIMES"; "3"; "."; "5"; "RPAR"; "POWER"; "LPAR"; "5"; "MINUS"; "3"; "RPAR"],
      [Number (Value {re = 1.5; im = 0.}); Op "PLUS"; Lpar; Number (Value {re = 2.5; im = 0.}) ; Op "TIMES";
      Number (Value {re = 3.5; im = 0.}); Rpar; Op "POWER"; Lpar; Number (Value {re = 5.; im = 0.});
      Op "MINUS"; Number (Value {re = 3.; im = 0.}); Rpar]);
    (["ABS"; "LPAR"; "LPAR"; "3"; "."; "RPAR"; "RPAR"; "EXCLAMATIONMARK"],
      [Lunop "ABS"; Lpar; Lpar; Number (Value {re = 3.; im = 0.}); Rpar; Rpar; Runop "EXCLAMATIONMARK"]);
    (["2"; "POWER"; "2"; "POWER"; "2"; "POWER"; "2"],
      [Number (Value {re = 2.; im = 0.}); Op "POWER"; Number (Value {re = 2.; im = 0.});
      Op "POWER"; Number (Value {re = 2.; im = 0.}); Op "POWER"; Number (Value {re = 2.; im = 0.})]);
    (["ABS"; "ABS"; "ABS"; "5"; "."; "EXCLAMATIONMARK"],
      [Lunop "ABS"; Lunop "ABS"; Lunop "ABS"; Number (Value {re = 5.; im = 0.}); Runop "EXCLAMATIONMARK"]);
    (["LPAR"; "ABS"; "5"; "."; "RPAR"], [Lpar; Lunop "ABS"; Number (Value {re = 5.; im = 0.}); Rpar]);
    (["1"; "PLUS"; "2"; "TIMES"; "CPLXI"],
      [Number (Value {re = 1.; im = 0.}); Op "PLUS"; Number (Value {re = 2.; im = 0.}); Op "TIMES";
      Number (Value {re = 0.; im = 1.})]);
    (["2"; "TIMES"; "B"; "PLUS"; "A"],
      [Number (Value {re = 2.; im = 0.}); Op "TIMES"; Number (Variable (Var 1)); Op "PLUS"; Number (Variable (Var 0))]);
    (["2"; "B"; "PLUS"; "3"; "A"; "MINUS"; "4"; "CPLXI"],
      [Number (Value {re = 2.; im = 0.}); Number (Variable (Var 1)); Op "PLUS"; Number (Value {re = 3.; im = 0.});
      Number (Variable (Var 0)); Op "MINUS"; Number (Value {re = 4.; im = 0.}); Number (Value {re = 0.; im = 1.})])
    ];
    print_endline "Tests_arithmetic_parsing, lexer: all tests passed"
  with
    | Test_failed i ->
      print_endline ("Tests_arithmetic_parsing, lexer: test "^(string_of_int i)^" failed")
    | Failure s ->
      print_endline ("Tests_arithmetic_parsing, lexer: one test encountered an error \""^s^"\"");;

let unit_tests_eval () =
  let var = Array.make (2*29) 0. in
  (* 10 -> A *)
  var.(0) <- 10.;
  (* 3+4i -> B *)
  var.(1) <- 3.;
  var.(1+29) <- 4.;

  let (proj : project_content) =
    {
      prog = [];
      list = Array.make 26 (true, [||]);
      mat = Array.make 26 (true, [||]);
      pict = Array.make 20 (0, [||]);
      capt = Array.make 20 [||];
      str = [||]
    }
  in

  let (p : parameters) = {
    (* proj: Contains the lists, matrices, pictures, captures and strings *)
    proj = proj;
    
    (* Variables: array of size 2*29, storing the content of each variable A..Z, r, theta, Ans
      as real part in the 29 first cells, and imaginary part in the next 29 *)
    var = Array.make (2*29) 0.;
    
    (* Complex numbers are represented in polar form if true, in carthesian form (a+ib) otherwise *)
    polar = false;

    (* Parameters of the V-Window *)
    xmin = 1.;
    xmax = 127.;
    xstep = 0.;
    ymin = 1.;
    ymax = 63.;
    ystep = 0.;
    (* Display the axes if true *)
    axes = false;

  } in


  let check i (slist, expected) =
    let (expr, _) = extract_expr slist in
    let z = eval p expr in
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
      (["2"; "B"; "PLUS"; "3"; "A"; "MINUS"; "4"; "CPLXI"], {re = 36.; im = 4.})
    ];
    print_endline "Tests_arithmetic_parsing, eval: all tests passed"
  with
    | Test_failed i ->
      print_endline ("Tests_arithmetic_parsing, eval: test "^(string_of_int i)^" failed")
    | Failure s ->
      print_endline ("Tests_arithmetic_parsing, eval: one test encountered an error \""^s^"\"");;



unit_tests_lexer ();;
unit_tests_eval ();;