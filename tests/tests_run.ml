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
      String ["A"; "B"; "C"; "D"];
      Disp;
      Expr (Arithm [Number (Value {re = 2.; im = 0.})]);
      Disp;
      Expr (Arithm [Number (Value {re = 1.; im = 2.})]);
      Disp;
      Expr (Arithm [Number (Value {re = 123456789012.; im = 2222.})]);
      Disp;
      Expr (Arithm [Number (Value {re = 123456789012.; im = 22222.})]);
      Disp;
      String ["E"; "F"; "G"; "H"];
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
        ["A"; "B"; "C"]);
      Disp;
      Next;
      String ["T"; "H"; "E"; " "; "E"; "N"; "D"];
      Disp;
      End
    |]
    ,
    [("main", 0)]);;

(* Subroutine calls *)
let run_prog4 () =
  run p (prog4 ());;


run p ([|
  Locate (Arithm [Number (Value {re = 10.; im = 0.})],
    Arithm [Number (Value {re = 5.; im = 0.})],
    ["A"; "B"; "C"]);
    Disp;
    End
  |],
  [("main", 0)]);;