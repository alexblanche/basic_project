(* Tests for basic_run *)

(* Empty project_content *)
let (p : project_content) =
  {
    prog = [];
    list = Array.make 26 (true, [||]);
    mat = Array.make 26 (true, [||]);
    pict = Array.make 20 (0, [||]);
    capt = Array.make 20 [||];
    str = [||]
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



(* To do: test subroutine calls *)