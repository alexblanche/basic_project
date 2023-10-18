(* Tests for basic_run *)

#use "tests/tests_compilation.ml"

(* Empty project_content *)
let empty_projcont () : project_content = 
  {
    prog = [];
    list = Array.make 27 (true, [||]);
    mat = Array.make 27 (true, [||]);
    pict = Array.make 20 (0, [||]);
    capt = Array.make 20 [||];
    str = Array.make 20 "";
  };;

(* Display *)
let run_prog1 () =
  run (empty_projcont ()) (
    [|
      Expr (Arithm [Entity (Value {re = 1.; im = 0.})]);
      Disp;
      String (Str_content ["D"; "C"; "B"; "A"]);
      Disp;
      Expr (Arithm [Entity (Value {re = 2.; im = 0.})]);
      Disp;
      Expr (Arithm [Entity (Value {re = 1.; im = 2.})]);
      Disp;
      Expr (Arithm [Entity (Value {re = 123456789012.; im = 2222.})]);
      Disp;
      Expr (Arithm [Entity (Value {re = 123456789012.; im = 22222.})]);
      Disp;
      String (Str_content ["H"; "G"; "F"; "E"]);
      Disp; 
      Expr (Arithm [Entity (Value {re = 5.; im = 0.})]);
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
      Expr (Arithm [Entity (Variable (Var 0)); Op "TIMES"; Entity (Variable (Var 1))]); (* A*B DISP *)
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
      Expr (Arithm [Entity (Variable (Var 1)); Op "TIMES"; Entity (Value {re = 2.; im = 0.})]); (* B*2 DISP *)
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

(********************************************************************)

let run_end () =
  let prog =
    compile
      [("main",
        [
         "1"; "PLUS"; "2"; "DISP";
         "QUOTE"; "A"; "B"; "C"; "QUOTE"; "PLUS"; "QUOTE"; "D"; "E"; "F"; "QUOTE"
        ]
      )]
  in
  run (empty_projcont ()) prog "main";;

let run_clrlist () =
  let prog =
    compile
      [("main",
        [
         "LBRACKET"; "1"; ","; "2"; "ASSIGN"; "LIST"; "2"; "EOL";
         "DIM"; "LIST"; "1"; "DISP";
         "DIM"; "LIST"; "2"; "DISP";
         "CLRLIST"; "EOL";
         "DIM"; "LIST"; "1"; "DISP";
         "DIM"; "LIST"; "2"; "DISP";
        ]
      )]
  in
  run (empty_projcont ()) prog "main";;

let run_seq () =
  let prog =
    compile
      [("main",
        [
         (* "SEQ"; "2"; "X"; ","; "X"; ","; "MINUS"; "0"; "."; "4"; ","; "5"; "."; "6"; ","; "1"; "."; "3"; "ASSIGN"; "LIST"; "1"; "EOL"; *)
         (* "SEQ"; "2"; "X"; ","; "X"; ","; "8"; "."; "7"; ","; "MINUS"; "0"; "."; "1"; ","; "MINUS"; "1"; "."; "1"; "ASSIGN"; "LIST"; "1"; "EOL"; *)
         (* "SEQ"; "X"; ","; "X"; ","; "1"; ","; "9"; ","; "1"; "ASSIGN"; "LIST"; "1"; "EOL"; *)
         "LSQBRACKET"; "LSQBRACKET"; "1"; ","; "8"; "8"; "8"; "RSQBRACKET"; "LSQBRACKET"; "3"; ","; "9"; "9"; "9"; "RSQBRACKET"; "RSQBRACKET"; "EOL";
         "MATTOLIST"; "ANS"; ","; "2"; "RPAR"; "EOL";
         (* "LBRACKET"; "1"; ","; "2"; ","; "3"; ","; "4"; "4"; "4"; "EOL"; *)
         "QUOTE"; "DIM"; "COLON"; "QUOTE"; "COLON"; "DIM"; "LIST"; "ANS"; "DISP";
         "LIST"; "ANS"; "LSQBRACKET"; "1"; "DISP";
         "LIST"; "ANS"; "LSQBRACKET"; "2"; "DISP";
         (* "LIST"; "ANS"; "LSQBRACKET"; "3"; "DISP";
         "LIST"; "ANS"; "LSQBRACKET"; "4"; "DISP"; *)
         (* "LIST"; "1"; "LSQBRACKET"; "5"; "DISP";
         "LIST"; "1"; "LSQBRACKET"; "6"; "DISP";
         "LIST"; "1"; "LSQBRACKET"; "7"; "DISP";
         "LIST"; "1"; "LSQBRACKET"; "8"; "DISP";
         "LIST"; "1"; "LSQBRACKET"; "9"; "DISP"; *)
        ]
      )]
  in
  (* prog;; *)
  (prog, run (empty_projcont ()) prog "main");;

(* Debug for a condition in the PAC-MAN game *)
let run_test_cond () =
  let e = Arithm
   [Lunop ("NOT", true);
    Lpar;
      Entity (Value {re = 0.; im = 0.});
      Op "OR";
      Lpar;
          Entity (Value {re = 1.; im = 0.});
          Op "AND";
          Entity (Variable (Var 3));
          Op "EQUAL";
          Lunop ("UMINUS", true); Entity (Value {Complex.re = 1.; im = 0.});
      Rpar;
    Rpar]
  in
  eval_num
    (let p = empty_param () in
    p.var.(3) <- 1.;
    p.var.(23) <- 5.;
    p.var.(24) <- 3.;
    p.list.(2) <-
      [|2.; 5.; 3.; 4.; 1.; 2.; 8.; 7.;
        0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
    p)
    e;;

let run_prog_minus () =
  let prog =
    compile
      [("main",
        [
         "NOT"; "LPAR"; "1"; "OR"; "LPAR"; "1"; "AND"; "1"; "EQUAL"; "MINUS"; "1"; "DISP"
        ]
      )]
  in prog;;
  (* run (empty_projcont ()) prog "main";; *)

let run_cond_list () =
  let prog =
    compile
      [("main",
        [
            "4"; "DIFFERENT"; "4"; "MINUS"; "1"; "AND"; "0"
        ]
      )]
  in (prog, run (empty_projcont ()) prog "main");;

let run_str_mid () =
  let prog =
    compile
      [("main",
        [
          "LOCATE"; "3"; ","; "5"; ","; "STRMID"; "QUOTE"; "0"; "1"; "2"; "3"; "4"; "5"; "6"; "QUOTE"; ","; "5"; "RPAR"; "DISP";
          "STRMID";
            "QUOTE"; "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K"; "L"; "M";
              "N"; "O"; "P"; "Q"; "R"; "S"; "T"; "U"; "V"; "W"; "X"; "Y"; "Z"; "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0"; "0";  "QUOTE"; ",";
              "5"; "RPAR"; "ASSIGN"; "STR"; "1"; "EOL";
          (* "STRLEFT"; "QUOTE"; "A"; "B"; "C"; "D"; "E"; "F"; "QUOTE"; ","; "2"; "RPAR"; "ASSIGN"; "STR"; "1"; "EOL"; *)
          "LOCATE"; "3"; ","; "3"; ","; "STR"; "1"; "DISP";
        ]
      )]
  in run (empty_projcont ()) prog "main";;

(************************************************************)

(** Tests for graphics **)

let run_prog_graphic () =
  let prog =
    compile
      [("main",
        [
          "VIEWWINDOW"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "1"; ","; "6"; "3"; ","; "0"; "EOL";
          "PLOTON"; "2"; "5"; ","; "1"; "0"; "EOL";
          "PLOTON"; "3"; "5"; ","; "6"; "0"; "EOL";
          "PLOTON"; "1"; "0"; "5"; ","; "1"; "0"; "EOL";
          "FLINE"; "1"; "0"; ","; "1"; "0"; ","; "1"; "0"; "0"; ","; "5"; "0"; "DISP";
          "TEXT"; "4"; "4"; ","; "5"; "0"; ","; "QUOTE"; "F"; "F"; "F"; "F"; "QUOTE"; "EOL";
          "TEXT"; "1"; ","; "1"; ","; "QUOTE"; "A"; "B"; "C"; "D"; "QUOTE"; "EOL";
          "TEXT"; "4"; "8"; ","; "1"; "1"; "6"; ","; "1"; "0"; "TIMES"; "5"; "3"; "2"; "EOL";
          "TEXT"; "5"; "8"; ","; "1"; "1"; "7"; ","; "1"; "0"; "TIMES"; "5"; "3"; "2"; "DISP";
          "FLINE"; "1"; "0"; ","; "3"; "0"; ","; "1"; "2"; "7"; ","; "4"; "0"; "DISP";
          "QUOTE"; "T"; "E"; "X"; "T"; "QUOTE"; "DISP";
          "PLOTON"; "1"; ","; "1"; "DISP";
          "QUOTE"; "S"; "T"; "O"; "P"; "QUOTE"; "DISP"
        ]
      )]
  in run (empty_projcont ()) prog "main";;

let run_pict () =
  let prog =
    compile
      [("main",
        [
          "RCLPICT"; "4"; "EOL";
        ]
      )]
  in
  let p =
    let par = empty_projcont () in
    let m = Array.make_matrix 64 128 false in
    for i = 2 to 100 do
      m.(i/2).(1+i) <- true;
      m.(63-i/2).(1+i+10) <- true
    done;
    par.pict.(3) <- (2048, m);
    par
  in
  run p prog "main";;

let run_window () =
  let prog =
    compile
      [("main",
        [ "AXESON"; "EOL";
          (* "BGPICT"; "4"; "EOL"; *)
          "VIEWWINDOW"; "MINUS"; "2"; ","; "1"; "8"; ","; "1"; ","; "MINUS"; "9"; ","; "1"; ","; "1"; "EOL";
          "PLOTON"; "2"; ","; "1"; "DISP";
          (* "TEXT"; "5"; "0"; "PLUS"; "2"; ","; "1"; "0"; ","; "QUOTE"; "A"; "B"; "C"; "D"; "QUOTE"; "EOL";
          "TEXT"; "1"; "0"; "PLUS"; "2"; ","; "1"; "0"; ","; "1"; "0"; "TIMES"; "5"; "3"; "2"; "DISP"; *)
        ]
      )]
  in
  let p =
    let par = empty_projcont () in
    (* let m = Array.make_matrix 64 128 false in
    for i = 2 to 100 do
      m.(i/2).(1+i) <- true;
      m.(63-i/2).(1+i+10) <- true
    done;
    par.pict.(3) <- (2048, m); *)
    par
  in 
  run p prog "main";;

let run_bounds () =
  let prog =
    compile
      [("main",
        [
          "VIEWWINDOW"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "1"; ","; "6"; "3"; ","; "0"; "EOL";
          "PLOTON"; "1"; ","; "3"; "EOL";
          "PLOTON"; "3"; ","; "3"; "EOL";
          "FLINE"; "1"; ","; "1"; ","; "3"; ","; "1"; "EOL";
          "PLOTON"; "1"; "2"; "5"; ","; "3"; "EOL";
          "PLOTON"; "1"; "2"; "7"; ","; "3"; "EOL";
          "FLINE"; "1"; "2"; "5"; ","; "1"; ","; "1"; "2"; "7"; ","; "1"; "DISP";
        ]
      )]
  in run (empty_projcont ()) prog "main";;

let run_vw () =
  let prog =
    compile
      [("main",
        [
          "VIEWWINDOW"; "MINUS"; "0"; "."; "0"; "1"; ","; "0"; "."; "0"; "1"; ","; "0"; "."; "0"; "0"; "5"; ","; "MINUS"; "1"; ","; "1"; ","; "0"; "."; "2"; "EOL";
          "AXESON"; "EOL";
          "PLOTON"; "0"; "."; "0"; "0"; "5"; ","; "0"; "."; "5"; "EOL"; "DISP";

          "CLS"; "EOL";
          "VIEWWINDOW"; "MINUS"; "5"; ","; "2"; ","; "0"; "."; "5"; ","; "MINUS"; "2"; ","; "8"; ","; "0"; "."; "5"; "EOL";
          "TEXT"; "3"; "0"; ","; "1"; "0"; ","; "QUOTE"; "MINUS"; "5"; ","; "2"; ","; "0"; "."; "5"; ","; "MINUS"; "2"; ","; "8"; ","; "0"; "."; "5"; "QUOTE"; "EOL";
          "PLOTON"; "0"; "."; "1"; ","; "1"; "EOL";
          "PLOTON"; "0"; "."; "1"; ","; "MINUS"; "1"; "DISP";

          "CLS"; "EOL";
          "VIEWWINDOW"; "MINUS"; "8"; ","; "MINUS"; "2"; ","; "1"; ","; "0"; ","; "8"; ","; "1"; "EOL";
          "TEXT"; "3"; "0"; ","; "1"; "0"; ","; "QUOTE"; "MINUS"; "8"; ","; "MINUS"; "2"; ","; "1"; ","; "0"; ","; "8"; ","; "1"; "QUOTE"; "EOL";
          "PLOTON"; "1"; ","; "1"; "DISP";
          
          "CLS"; "EOL";
          "VIEWWINDOW"; "MINUS"; "5"; ","; "5"; ","; "1"; ","; "MINUS"; "5"; ","; "0"; ","; "1"; "EOL";
          "TEXT"; "3"; "0"; ","; "1"; "0"; ","; "QUOTE"; "MINUS"; "5"; ","; "5"; ","; "1"; ","; "MINUS"; "5"; ","; "0"; ","; "1"; "QUOTE"; "EOL";
          "PLOTON"; "1"; ","; "1"; "DISP";

          "CLS"; "EOL";
          "VIEWWINDOW"; "MINUS"; "5"; ","; "5"; ","; "1"; ","; "MINUS"; "5"; ","; "0"; "."; "1"; ","; "1"; "EOL";
          "TEXT"; "3"; "0"; ","; "1"; "0"; ","; "QUOTE"; "MINUS"; "5"; ","; "5"; ","; "1"; ","; "MINUS"; "5"; ","; "0"; "."; "1"; ","; "1"; "QUOTE"; "EOL";
          "PLOTON"; "1"; ","; "1"; "DISP";
        ]
      )]
  (* in prog ;;  *)
  in run (empty_projcont ()) prog "main";;

let run_drawstat () =
  let prog =
    compile
      [("main",
        [
          "AXESON"; "EOL";
          "VIEWWINDOW"; "MINUS"; "5"; ","; "2"; ","; "0"; "."; "5"; ","; "MINUS"; "2"; ","; "8"; ","; "0"; "."; "5"; "EOL";
          "LBRACKET"; "MINUS"; "4"; ","; "MINUS"; "1"; ","; "MINUS"; "2"; ","; "MINUS"; "3"; ","; "MINUS"; "1"; "RBRACKET"; "ASSIGN"; "LIST"; "1"; "EOL";
          "LBRACKET"; "1"; ","; "3"; ","; "7"; ","; "5"; ","; "1"; "RBRACKET"; "ASSIGN"; "LIST"; "2"; "EOL";
          "SGPH1"; "DRAWON"; ","; "XYLINE"; ","; "LIST"; "1"; ","; "LIST"; "2"; ","; "1"; ","; "DOT"; "EOL";
          "DRAWSTAT"; "DISP";

          "CLS"; "EOL";
          "SGPH1"; "DRAWON"; ","; "XYLINE"; ","; "LIST"; "1"; ","; "LIST"; "2"; ","; "1"; ","; "CROSS"; "EOL";
          "DRAWSTAT"; "DISP";

          "CLS"; "EOL";
          "SGPH1"; "DRAWON"; ","; "XYLINE"; ","; "LIST"; "1"; ","; "LIST"; "2"; ","; "1"; ","; "SQUARE"; "EOL";
          "DRAWSTAT"; "DISP";

          "CLS"; "EOL";
          "SGPH1"; "DRAWON"; ","; "SCATTER"; ","; "LIST"; "1"; ","; "LIST"; "2"; ","; "1"; ","; "SQUARE"; "EOL";
          "DRAWSTAT"; "DISP";
        ]
      )]
  (* in prog ;;  *)
  in run (empty_projcont ()) prog "main";;

let run_bgpict () =
  let prog =
    compile
      [("main",
        [
          "VIEWWINDOW"; "MINUS"; "0"; "."; "0"; "1"; ","; "0"; "."; "0"; "1"; ","; "0"; "."; "0"; "0"; "5"; ","; "MINUS"; "1"; ","; "1"; ","; "0"; "."; "2"; "EOL";
          "AXESON"; "EOL";
          "BGPICT"; "4";
          "PLOTON"; "0"; "."; "0"; "0"; "5"; ","; "0"; "."; "5"; "EOL"; "DISP";

          "CLS"; "EOL";
          "VIEWWINDOW"; "MINUS"; "5"; ","; "2"; ","; "0"; "."; "5"; ","; "MINUS"; "2"; ","; "8"; ","; "0"; "."; "5"; "EOL";
          "TEXT"; "3"; "0"; ","; "1"; "0"; ","; "QUOTE"; "MINUS"; "5"; ","; "2"; ","; "0"; "."; "5"; ","; "MINUS"; "2"; ","; "8"; ","; "0"; "."; "5"; "QUOTE"; "EOL";
          "PLOTON"; "0"; "."; "1"; ","; "1"; "EOL";
          "PLOTON"; "0"; "."; "1"; ","; "MINUS"; "1"; "DISP";

          "RCLCAPT"; "8"; "DISP";
          "FLINE"; "1"; ","; "6"; ","; "2"; ","; "6"; "."; "5"; "EOL"
        ])]
  in
  let p =
    let par = empty_projcont () in
    let m = Array.make_matrix 64 128 false in
    for i = 2 to 100 do
      m.(i/2).(1+i) <- true;
      m.(63-i/2).(1+i+10) <- true
    done;
    par.pict.(3) <- (2048, m);
    let c = Array.make_matrix 64 128 false in
    for i = 2 to 100 do
      c.(10).(i) <- true;
      c.(50).(i/2) <- true;
      c.(50).(i/2 + 60) <- true
    done;
    par.capt.(7) <- c;
    par
  in
  run p prog "main";;

let run_frame () =
  let prog =
    compile
      [("main",
        [
          "VIEWWINDOW"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "1"; ","; "6"; "3"; ","; "0"; "EOL";
          "TEXT"; "5"; "8"; ","; "0"; "2"; "5"; ","; "QUOTE"; "A"; "B"; "C"; "QUOTE"; "EOL";
          "VERTICAL"; "3" ; "6"; "EOL";
          "HORIZONTAL"; "6"; "3"; "DISP";
          "TEXT"; "5"; "8"; ","; "0"; "2"; "5"; ","; "QUOTE"; "A"; "B"; "C"; "QUOTE"; "EOL";
          "TEXT"; "5"; "8"; ","; "1"; "2"; "4"; ","; "QUOTE"; "A"; "B"; "C"; "QUOTE"; "EOL";
        ]
      )]
  in run (empty_projcont ()) prog "main";;