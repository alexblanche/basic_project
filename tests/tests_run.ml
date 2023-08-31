(* Tests for basic_run *)

let (p : project_content) =
  {
    prog = [];
    list = Array.make 26 (true, [||]);
    mat = Array.make 26 (true, [||]);
    pict = Array.make 20 (0, [||]);
    capt = Array.make 20 [||];
    str = [||]
  };;
(* 
let prog_a =
  compile
    [("main",
    [
      "QUOTE"; "A"; "A"; "A"; "QUOTE"; "DISP";
      "1"; "DISP";
      "2"; "DISP";
      "1"; "PLUS"; "2"; "CPLXI"; "DISP";
      "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "0"; "1"; "2"
        "PLUS"; "2"; "2"; "2"; "2"; "CPLXI"; "DISP";
      "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "0"; "1"; "2"
        "PLUS"; "2"; "2"; "2"; "2"; "2"; "CPLXI"; "DISP"
    ]
    )] in

run p prog_a;; *)

run p (
  [|
    Expr (Arithm [Number (Value {re = 1.; im = 0.})]);
    Disp;
    Expr (Arithm [Number (Value {re = 2.; im = 0.})]);
    Disp;
    Expr (Arithm [Number (Value {re = 1.; im = 2.})]);
    Disp;
    Expr (Arithm [Number (Value {re = 123456789012.; im = 2222.})]);
    Disp;
    Expr (Arithm [Number (Value {re = 123456789012.; im = 22222.})]);
    Disp;
  |]
  ,
  [("main", 0)]);;