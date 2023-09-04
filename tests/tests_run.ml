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

(* To do: test subroutine calls *)