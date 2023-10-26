(* Unit tests for float_repr.ml *)

(* #use "basic_parsing/float_repr.ml" *)

exception Test_failed of int

let unit_tests () =
  let check i (nocaml,scasio) =
    (* print_endline ("Test n°"^(string_of_int i)^": "^scasio); *)
    let sresult = float_to_casio nocaml in
    if sresult <> scasio then
      (print_endline
        ("Expected: "^(String.map (fun c -> if c = '\015' then 'E' else c) scasio)
        ^", Result: "^(String.map (fun c -> if c = '\015' then 'E' else c) sresult));
      raise (Test_failed i))
  in
  try
    List.iteri check [
    (12345., "12345");
    (1234567891., "1234567891");
    (12345678912., "1.234567891\015+10");
    (123456789123., "1.234567891\015+11");
    (1e+15, "1\015+15");
    (1.23456789123, "1.234567891");
    (1.23456789123e+12, "1.234567891\015+12");
    (0.0124, "0.0124");
    (0.00124, "1.24\015-03");
    (0.0001, "1\015-04");
    (0.000001, "1\015-06");
    (1e-05, "1\015-05");
    (-12345., "-12345");
    (-1234567891., "-1234567891");
    (-12345678912., "-1.234567891\015+10");
    (-123456789123., "-1.234567891\015+11");
    (-1e+15, "-1\015+15");
    (-1.23456789123, "-1.234567891");
    (-1.23456789123e+12, "-1.234567891\015+12");
    (-0.0124, "-0.0124");
    (-0.00124, "-1.24\015-03");
    (-0.0001, "-1\015-04");
    (-1e-05, "-1\015-05");
    (123456789012., "1.23456789\015+11");
    (0., "0");
    (1., "1");
    (1e10, "1\015+10");
    (10000000.00234, "10000000");
    (1.000000000234e+25, "1\015+25");
    (1.000000000234e-13, "1\015-13");
    (152640406006912., "1.52640406\015+14");
    (1.20000000001, "1.2");
    (Float.pred 1e+12, "1\015+12");
    ];
    print_endline "----------------------------------";
    print_endline "Tests_float_repr: all tests passed";
    print_endline "----------------------------------"
  with
    | Test_failed i ->
      print_endline ("Tests_float_repr: test "^(string_of_int i)^" failed")
    | Failure s ->
      print_endline ("Tests_float_repr: one test encountered an error \""^s^"\"")
    | Invalid_argument s ->
      print_endline ("Tests_float_repr: one test encountered an error \""^s^"\"");;

unit_tests ();;