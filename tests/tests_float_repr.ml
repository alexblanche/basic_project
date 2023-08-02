(* Unit tests for float_repr.ml *)

#use "basic_parsing/float_repr.ml"

exception Test_failed of int

let unit_tests () =
  let check i (nocaml,scasio) =
    (* In order to be sure the number is properly formatted *)
    let socaml = float_of_string (string_of_float nocaml) in
    if float_to_casio socaml <> scasio
      then raise (Test_failed i)
  in
  try
    List.iteri check [
    (12345., "12345");
    (1234567891., "1234567891");
    (12345678912., "1.234567891e+10");
    (123456789123., "1.234567891e+11");
    (1e+15, "1e+15");
    (1.23456789123, "1.234567891");
    (1.23456789123e+12, "1.234567891e+12");
    (0.0124, "0.0124");
    (0.00124, "1.24e-03");
    (0.0001, "1e-04");
    (1e-05, "1e-05");
    (-.12345., "-12345");
    (-.1234567891., "-1234567891");
    (-.12345678912., "-1.234567891e+10");
    (-.123456789123., "-1.234567891e+11");
    (-.1e+15, "-1e+15");
    (-.1.23456789123, "-1.234567891");
    (-.1.23456789123e+12, "-1.234567891e+12");
    (-.0.0124, "-0.0124");
    (-.0.00124, "-1.24e-03");
    (-.0.0001, "-1e-04");
    (-.1e-05, "-1e-05")
    ];
    print_endline "Tests_float_repr: all tests ran successfully"
  with
    | Test_failed i ->
      print_endline ("Tests_float_repr: test "^(string_of_int i)^" failed");;

unit_tests ();;