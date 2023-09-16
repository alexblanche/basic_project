(* Tests for basic_run *)

#use "tests/tests_compilation.ml"

(* Empty project_content *)
let (p : project_content) =
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
  run p (
    [|
      Expr (Arithm [Number (Value {re = 1.; im = 0.})]);
      Disp;
      String ["D"; "C"; "B"; "A"];
      Disp;
      Expr (Arithm [Number (Value {re = 2.; im = 0.})]);
      Disp;
      Expr (Arithm [Number (Value {re = 1.; im = 2.})]);
      Disp;
      Expr (Arithm [Number (Value {re = 123456789012.; im = 2222.})]);
      Disp;
      Expr (Arithm [Number (Value {re = 123456789012.; im = 22222.})]);
      Disp;
      String ["H"; "G"; "F"; "E"];
      Disp; 
      Expr (Arithm [Number (Value {re = 5.; im = 0.})]);
      Disp;
      End
    |]
    ,
    [("main", 0)]);;

(* Variables *)
let run_prog2 () =
  run p (
    [|
      Assign (Arithm [Number (Value {re = 8.; im = 0.})], (Var 0)); (* 8 -> A DISP *)
      Disp;
      Assign (Arithm [Number (Value {re = 10.; im = 0.})], (Var 1)); (* 10 -> B DISP *)
      Disp;
      Expr (Arithm [Number (Variable (Var 0)); Op "TIMES"; Number (Variable (Var 1))]); (* A*B DISP *)
      Disp;
      End
    |]
    ,
    [("main", 0)]);;

(* For, QMark *)
let run_prog3 () =
  run p (
    [|
      For (0, (* For 3-1 -> A To 8 Step 2 *)
        Arithm [Number (Value {re = 3.; im = 0.}); Op "MINUS"; Number (Value {re = 1.; im = 0.})],
        Arithm [Number (Value {re = 8.; im = 0.})],
        Arithm [Number (Value {re = 2.; im = 0.})],
        7);
      Assign (QMark, Var 1); (* ? -> B *)
      Expr (Arithm [Number (Variable (Var 1)); Op "TIMES"; Number (Value {re = 2.; im = 0.})]); (* B*2 DISP *)
      Disp;
      Locate (Arithm [Number (Value {re = 10.; im = 0.})],
        Arithm [Number (Value {re = 5.; im = 0.})],
        Loc_text ["C"; "B"; "A"]);
      Disp;
      Next;
      String ["D"; "N"; "E"; " "; "E"; "H"; "T"];
      Disp;
      End
    |]
    ,
    [("main", 0)]);;

(* Subroutine calls *)
let run_prog4 () =
  run p (prog4 ());;

(* Test Locate *)
(* run p ([|
  Locate (Arithm [Number (Value {re = 10.; im = 0.})],
    Arithm [Number (Value {re = 5.; im = 0.})],
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
        "LOCATE"; "1"; "COMMA"; "2"; "COMMA"; "X"; "EOL";
        "NEXT";
        "QUOTE"; "D"; "O"; "N"; "E"; "QUOTE"; "DISP"
        ]
      )]
  in
  run p prog5;;

(* Result: about 10^8 operations (empty For Next) in 6 to 10s *)
(* With Locate, program runs 3 times faster than the calculator... *)
(* I hope that when compiled, it will be faster... *)

(* Getkey *)
let run_prog_getkey () =
  run p (prog_getkey ());;

(* Speed of a For loop *)
let run_prog6 () =
  let prog6 =
    compile
      [("main",
        ["QUOTE"; "R"; "E"; "A"; "D"; "Y"; "?"; "QUOTE"; "DISP";
        "FOR"; "1"; "ASSIGN"; "X"; "TO"; "1"; "TIMESTENPOWER"; "3"; "EOL";
        "QUOTE"; "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K"; "L"; "M";
          "N"; "O"; "P"; "Q"; "R"; "S"; "T"; "U";
          "V"; "W"; "X"; "Y"; "Z"; "QUOTE"; "EOL";
        "NEXT";
        "QUOTE"; "D"; "O"; "N"; "E"; "QUOTE"; "DISP"
        ]
      )]
  in
  run p prog6;;
