(* Tests for basic_run *)

#use "tests/tests_compilation.ml"

(* Empty project_content *)
let empty_projcont () : project_content = 
  {
    prog = [];
    list = Array.make 26 (true, [||]);
    mat = Array.make 26 (true, [||]);
    pict = Array.make 20 (0, [||]);
    capt = Array.make 20 [||];
    str = Array.make 20 "";
  };;

(* Display *)
let run_prog1 () =
  run (empty_projcont ()) (
    [|
      Expr (Arithm [Entity (Value {re = 1.; im = 0.})], Numerical);
      Disp;
      String (Str_content ["D"; "C"; "B"; "A"]);
      Disp;
      Expr (Arithm [Entity (Value {re = 2.; im = 0.})], Numerical);
      Disp;
      Expr (Arithm [Entity (Value {re = 1.; im = 2.})], Numerical);
      Disp;
      Expr (Arithm [Entity (Value {re = 123456789012.; im = 2222.})], Numerical);
      Disp;
      Expr (Arithm [Entity (Value {re = 123456789012.; im = 22222.})], Numerical);
      Disp;
      String (Str_content ["H"; "G"; "F"; "E"]);
      Disp; 
      Expr (Arithm [Entity (Value {re = 5.; im = 0.})], Numerical);
      Disp;
      End
    |]
    ,
    [("main", 0)])
  "main";;

(* Variables *)
let run_prog2 () =
  run (empty_projcont ()) (
    [|
      Assign (Arithm [Entity (Value {re = 8.; im = 0.})], (Var 0)); (* 8 -> A DISP *)
      Disp;
      Assign (Arithm [Entity (Value {re = 10.; im = 0.})], (Var 1)); (* 10 -> B DISP *)
      Disp;
      Expr (Arithm [Entity (Variable (Var 0)); Op "TIMES"; Entity (Variable (Var 1))], Numerical); (* A*B DISP *)
      Disp;
      End
    |]
    ,
    [("main", 0)])
  "main";;

(* For, QMark *)
let run_prog3 () =
  run (empty_projcont ()) (
    [|
      For (0, (* For 3-1 -> A To 8 Step 2 *)
        Arithm [Entity (Value {re = 3.; im = 0.}); Op "MINUS"; Entity (Value {re = 1.; im = 0.})],
        Arithm [Entity (Value {re = 8.; im = 0.})],
        Arithm [Entity (Value {re = 2.; im = 0.})],
        7);
      Assign (QMark, Var 1); (* ? -> B *)
      Expr (Arithm [Entity (Variable (Var 1)); Op "TIMES"; Entity (Value {re = 2.; im = 0.})], Numerical); (* B*2 DISP *)
      Disp;
      Locate (Arithm [Entity (Value {re = 10.; im = 0.})],
        Arithm [Entity (Value {re = 5.; im = 0.})],
        Str_content ["C"; "B"; "A"]);
      Disp;
      Next;
      String (Str_content ["D"; "N"; "E"; " "; "E"; "H"; "T"]);
      Disp;
      End
    |]
    ,
    [("main", 0)])
  "main";;

(* Subroutine calls *)
let run_prog4 () =
  run (empty_projcont ()) (prog4 ()) "MAIN";;

(* Test Locate *)
(* run p ([|
  Locate (Arithm [Entity (Value {re = 10.; im = 0.})],
    Arithm [Entity (Value {re = 5.; im = 0.})],
    ["A"; "B"; "C"]);
    Disp;
    End
  |],
  [("main", 0)]);; *)

(* Speed of a For loop *)
let run_prog5 () =
  let prog5 =
    compile
      [("main",
        ["QUOTE"; "R"; "E"; "A"; "D"; "Y"; "?"; "QUOTE"; "DISP";
        "FOR"; "1"; "ASSIGN"; "X"; "TO"; "1"; "TIMESTENPOWER"; "3"; "EOL";
        "LOCATE"; "1"; ","; "2"; ","; "X"; "EOL";
        "NEXT";
        "QUOTE"; "D"; "O"; "N"; "E"; "QUOTE"; "DISP"
        ]
      )]
  in
  run (empty_projcont ()) prog5 "main";;

(* Result: about 10^8 operations (empty For Next) in 6 to 10s *)
(* With Locate, program runs 3 times faster than the calculator... *)
(* I hope that when compiled, it will be faster... *)

(* Getkey *)
let run_prog_getkey () =
  run (empty_projcont ()) (prog_getkey ()) "main";;

(* Speed of a For loop *)
let run_prog6 () =
  let prog6 =
    compile
      [("main",
        ["QUOTE"; "R"; "E"; "A"; "D"; "Y"; "?"; "QUOTE"; "DISP";
        "FOR"; "1"; "ASSIGN"; "X"; "TO"; "1"; "TIMESTENPOWER"; "3"; "EOL";
        "QUOTE"; "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K"; "L"; "M";
          "N"; "O"; "P"; "Q"; "R"; "S"; "T"; "U"; "V"; "W"; "X"; "Y"; "Z"; "QUOTE"; "EOL";
        "QUOTE"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "0"; "QUOTE"; "EOL";
        "NEXT";
        "QUOTE"; "D"; "O"; "N"; "E"; "QUOTE"; "DISP"
        ]
      )]
  in
  run (empty_projcont ()) prog6 "main";;
  (* 22s in the emulator, 37s in the calculator *)

(* Test of IMPL => *)

let run_prog7 () =
  let prog7 =
    compile
      [("main",
        ["9"; "EOL";
        "3"; "EQUAL"; "2"; "IMPL"; "8"; "EOL";
        "ANS"; "DISP"
        ]
      )]
  in
  run (empty_projcont ()) prog7 "main";;

let run_prog_if () =
  run (empty_projcont ()) (prog_if ()) "main";;

(*******************************************)

(* Debug of Timeless *)
let run_prog_assign_list () =
  let prog_assign_list =
    compile
      [("main",
        [
        (* "5"; "ASSIGN"; "DIM"; "LIST"; "9"; "EOL"; *)
        (* "1"; "2"; "ASSIGN"; "LIST"; "9"; "LSQBRACKET"; "4"; "EOL"; *)
        (* "1"; "1"; "ASSIGN"; "U"; "EOL";
        "LBRACKET"; "1"; ","; "2"; "CPLXI"; "ASSIGN"; "LIST"; "U"; "EOL"; *)
        (* "LIST"; "9"; "LSQBRACKET"; "4"; "DISP"; *)
        (* "LIST"; "1"; "1"; "LSQBRACKET"; "2"; "DISP"; *)
        "1"; "ASSIGN"; "D"; "EOL";
        "7"; "PLUS"; "LPAR"; "D"; "MINUS"; "1"; "RPAR"; "INTDIV"; "1"; "3"; "ASSIGN"; "N"; "EOL";
        "N"; "DISP";
        "LIST"; "N"; "ASSIGN"; "LIST"; "1"; "0"; "EOL"
        ]
      )]
  in
  (prog_assign_list, run (empty_projcont ()) prog_assign_list "main");;

let run_prog_locate () =
  let prog =
    compile
      [("main",
        ["QUOTE";
          "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "a"; "a"; "a";
          "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "b"; "b"; "b";
          "C"; "c";
        "QUOTE"; "DISP"]
      )]
  in
  run (empty_projcont ()) prog "main";;

let run_prog_eval () =
  let prog =
    compile
      [("main",
        [
         "1"; "AND"; "1"; "DISP";
         "2"; "ASSIGN"; "D"; "EOL";
         "3"; "ASSIGN"; "THETA"; "EOL";
         "QUOTE"; "D"; "QUOTE"; "EOL";
            "D"; "DISP";
         "QUOTE"; "REP"; "D"; "QUOTE"; "EOL";
            "REP"; "D"; "DISP";
         "QUOTE"; "THETA"; "QUOTE"; "EOL";
            "THETA"; "DISP";
         "QUOTE"; "D"; "EQUAL"; "REP"; "D"; "QUOTE"; "EOL";
            "D"; "EQUAL"; "REP"; "D"; "DISP";
         "QUOTE"; "THETA"; "DIFFERENT"; "CPLXI"; "QUOTE"; "EOL";
            "THETA"; "DIFFERENT"; "CPLXI"; "DISP";
         "QUOTE"; "D"; "EQUAL"; "REP"; "D"; "AND"; "THETA"; "DIFFERENT"; "CPLXI"; "QUOTE"; "EOL";
         "D"; "EQUAL"; "REP"; "D"; "AND"; "THETA"; "DIFFERENT"; "CPLXI"; "DISP"]
      )]
  in
  run (empty_projcont ()) prog "main";;

let run_prog_print () =
  let prog =
    compile
      [("main",
        [
          "QUOTE"; " "; "*"; "*"; "*"; " "; "L"; "a"; "u"; "r"; "e"; "a"; "t"; " "; " "; "d"; "u"; " "; "*"; "*"; "*"; " "; "QUOTE"; "EOL";
          "QUOTE"; "C"; "o"; "n"; "c"; "o"; "u"; "r"; "s"; " "; "A"; "n"; "n"; "i"; "v"; "e"; "r"; "s"; "a"; "i"; "r"; "e"; "QUOTE"; "EOL";
          "QUOTE";
            " "; "P"; "l"; "a"; "n"; "e"; "t"; "e"; " "; "C"; "a"; "s"; "i"; "o"; " "; " "; "2"; "0"; "1"; "2"; " ";
            " "; "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*"; "QUOTE"; "EOL";
          "FOR"; "1"; "ASSIGN"; "X"; "TO"; "7"; "9"; "8"; "EOL";
          "NEXT"; "EOL";
          "CLRTEXT"; "EOL";

         "QUOTE";
            "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A";
            "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B";
            "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C";
            "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A";
            "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B";
            "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C";
         "QUOTE"; "EOL";
         "FOR"; "1"; "ASSIGN"; "X"; "TO"; "7"; "9"; "8"; "EOL";
         "NEXT"; "EOL";
         "QUOTE"; "A"; "QUOTE"; "EOL";
         "FOR"; "1"; "ASSIGN"; "X"; "TO"; "7"; "9"; "8"; "EOL";
         "NEXT"; "EOL";
         "CLRTEXT"; "EOL";
         "QUOTE";
            "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A";
            "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B";
            "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C";
            "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A";
            "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B";
            "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C";
            "D"; "D"; "D"; "D"; "D"; "D"; "D"; "D"; "D"; "D"; "D"; "D"; "D"; "D"; "D"; "D"; "D"; "D"; "D"; "D"; "D";
         "QUOTE"; "EOL";
         "FOR"; "1"; "ASSIGN"; "X"; "TO"; "7"; "9"; "8"; "EOL";
         "NEXT"; "EOL";
         "QUOTE"; "A"; "QUOTE"; "EOL";

         "FOR"; "1"; "ASSIGN"; "X"; "TO"; "7"; "9"; "8"; "EOL";
         "NEXT"; "EOL";
         "CLRTEXT"; "EOL";

         "QUOTE";
            "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A";
            "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B";
            "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C";
            "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A";
            "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B";
            "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C";
            "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A";
            "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B";
            "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C";
            "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A";
            "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B";
            "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C";
         "QUOTE"; "EOL";
         "FOR"; "1"; "ASSIGN"; "X"; "TO"; "7"; "9"; "8"; "EOL";
         "NEXT"; "EOL";
         "QUOTE"; "A"; "QUOTE"; "EOL";

         "FOR"; "1"; "ASSIGN"; "X"; "TO"; "7"; "9"; "8"; "EOL";
         "NEXT"; "EOL";

         "CLRTEXT"; "EOL";

         "QUOTE";
            "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A";
            "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B";
            "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C";
            "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A";
            "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B";
            "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C";
            "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A";
            "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B";
            "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C";
            "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A";
            "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B";
            "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C";
         "QUOTE"; "EOL";
         "FOR"; "1"; "ASSIGN"; "X"; "TO"; "7"; "9"; "8"; "EOL";
         "NEXT"; "EOL";
         "QUOTE"; "A"; "QUOTE"; "EOL";
         "FOR"; "1"; "ASSIGN"; "X"; "TO"; "7"; "9"; "8"; "EOL";
         "NEXT"; "EOL";
         

         "CLRTEXT"; "EOL";
         "QUOTE";
            "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A";
            "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B";
            "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C";
            "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A";
            "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B";
            "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C"; "C";
            "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A";
         "QUOTE"; "EOL";
         "FOR"; "1"; "ASSIGN"; "X"; "TO"; "7"; "9"; "8"; "EOL";
         "NEXT"; "EOL";
         "QUOTE"; "A"; "QUOTE"; "EOL"
        ]
      )]
  in
  run (empty_projcont ()) prog "main";;