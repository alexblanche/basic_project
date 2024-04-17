(* Tests for basic_run *)

#use "tests/tests_compilation.ml"

(** Test functions **)

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

(* Main function *)
let main_test (codeprog : basic_code) (verbose : bool) : unit =

  let (_, progl) = codeprog in
  let (c : content) =
    {
      prog = List.map (fun (s, i) -> (s, i, 0, 0)) progl;
      list = []; mat = []; pict = [];
      capt = []; str = []
    }
  in
  let (p : project_content) = empty_projcont () in
  main_menu codeprog p c verbose;;

(* Test run function, with verbose = true *)
let run_test (prog : basic_code) : unit =
  main_test prog true;;

(* Test run function, with specified project_content *)
let run_with_projcont (p : project_content) (codeprog : basic_code) : unit =
  let (_, progl) = codeprog in
  let (c : content) =
    {
      prog = List.map (fun (s, i) -> (s, i, 0, 0)) progl;
      list = []; mat = []; pict = [];
      capt = []; str = []
    }
  in
  main_menu codeprog p c true;;

(*********************************************************************************************)

(* Display *)
let run_prog1 () =
  run_test (
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
    [("main", 0)]);;

(* Variables *)
let run_prog2 () =
  run_test (
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
    [("main", 0)]);;

(* For, QMark *)
let run_prog3 () =
  run_test (
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
    [("main", 0)]);;

(* Subroutine calls *)
let run_prog4 () =
  run_test (prog4 ());;

(* Speed of a For loop *)
let run_prog5 () =
  let prog5 =
    compile_test
      [("main",
        ["QUOTE"; "R"; "E"; "A"; "D"; "Y"; "?"; "QUOTE"; "DISP";
        "FOR"; "1"; "ASSIGN"; "X"; "TO"; "1"; "TIMESTENPOWER"; "3"; "EOL";
        "LOCATE"; "1"; ","; "2"; ","; "X"; "EOL";
        "NEXT";
        "QUOTE"; "D"; "O"; "N"; "E"; "QUOTE"; "DISP"
        ]
      )]
  in
  run_test prog5;;

(* Result: about 10^8 operations (empty For Next) in 6 to 10s *)
(* With Locate, program runs 3 times faster than the calculator... *)
(* I hope that when compiled, it will be faster... *)

(* Getkey *)
let run_prog_getkey () =
  run_test (prog_getkey ());;

(* Speed of a For loop *)
let run_prog6 () =
  let prog6 =
    compile_test
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
  run_test prog6;;
  (* 22s in the emulator, 37s in the calculator *)

(* Test of IMPL => *)

let run_prog7 () =
  let prog7 =
    compile_test
      [("main",
        ["9"; "EOL";
        "3"; "EQUAL"; "2"; "IMPL"; "8"; "EOL";
        "ANS"; "DISP"
        ]
      )]
  in
  run_test prog7;;

let run_prog_if () =
  run_test (prog_if ());;

(*******************************************)

(* Debug of Timeless *)
let run_prog_assign_list () =
  let prog_assign_list =
    compile_test
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
  (prog_assign_list, run_test prog_assign_list);;

let run_prog_locate () =
  let prog =
    compile_test
      [("main",
        ["QUOTE";
          "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "a"; "a"; "a";
          "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "b"; "b"; "b";
          "C"; "c";
        "QUOTE"; "DISP"]
      )]
  in
  run_test prog;;

let run_prog_eval () =
  let prog =
    compile_test
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
  run_test prog;;

let run_prog_print () =
  let prog =
    compile_test
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
  run_test prog;;

(********************************************************************)

let run_end () =
  let prog =
    compile_test
      [("main",
        [
         "1"; "PLUS"; "2"; "DISP";
         "QUOTE"; "A"; "B"; "C"; "QUOTE"; "PLUS"; "QUOTE"; "D"; "E"; "F"; "QUOTE"
        ]
      )]
  in
  run_test prog;;

let run_clrlist () =
  let prog =
    compile_test
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
  run_test prog;;

let run_seq () =
  let prog =
    compile_test
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
  (prog, run_test prog);;

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
    compile_test
      [("main",
        [
         "NOT"; "LPAR"; "1"; "OR"; "LPAR"; "1"; "AND"; "1"; "EQUAL"; "MINUS"; "1"; "DISP"
        ]
      )]
  in prog;;
  (* run_test prog;; *)

let run_cond_list () =
  let prog =
    compile_test
      [("main",
        [
            "4"; "DIFFERENT"; "4"; "MINUS"; "1"; "AND"; "0"
        ]
      )]
  in (prog, run_test prog);;

let run_str_mid () =
  let prog =
    compile_test
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
  in run_test prog;;

(************************************************************)

(** Tests for graphics **)

let run_prog_graphic () =
  let prog =
    compile_test
      [("main",
        [ (* First tests of ViewWindow, PlotOn, F-line, Text, going back and forth between
           the tscreen and the gscreen *)
          "VIEWWINDOW"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "1"; ","; "6"; "3"; ","; "0"; "EOL";
          "VERTICAL"; "5"; "TIMES"; "1"; "0"; "EOL";
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
  in run_test prog;;

let run_pict () =
  let prog =
    compile_test
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
      m.(5).(i) <- true;
      m.(i/2).(1+i) <- true;
      m.(63-i/2).(1+i+10) <- true
    done;
    par.pict.(3) <- (2048, m);
    par
  in
  run_with_projcont p prog;;

let run_window () =
  let prog =
    compile_test
      [("main",
        [ "AXESON"; "EOL";
          (* "BGPICT"; "4"; "EOL"; *)
          "VIEWWINDOW"; "MINUS"; "2"; ","; "1"; "8"; ","; "1"; ","; "MINUS"; "9"; ","; "1"; ","; "1"; "EOL";
          "PLOTON"; "2"; ","; "1"; "EOL";
          "PXLON"; "5"; "0"; ","; "1"; "4"; "EOL";
          "PXLOFF"; "5"; "0"; ","; "1"; "3"; "DISP";
          "PXLCHG"; "5"; "0"; ","; "1"; "2"; "EOL";
          "PXLCHG"; "4"; "4"; ","; "1"; "3"; "EOL";
          "TEXT"; "5"; "0"; "PLUS"; "2"; ","; "1"; "4"; ","; "QUOTE"; "A"; "B"; "C"; "D"; "QUOTE"; "EOL";
          "TEXT"; "1"; "0"; "PLUS"; "2"; ","; "1"; "0"; ","; "1"; "0"; "TIMES"; "5"; "3"; "2"; "DISP";
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
  run_with_projcont p prog;;

let run_bounds () =
  let prog =
    compile_test
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
  in
  run_test prog;;

let run_vw () =
  let prog =
    compile_test
      [("main",
        [ (* Checking that the axes are pixel perfect *)
          "VIEWWINDOW"; "MINUS"; "0"; "."; "0"; "1"; ","; "0"; "."; "0"; "1"; ","; "0"; "."; "0"; "0"; "5"; ","; "MINUS"; "1"; ","; "1"; ","; "0"; "."; "2"; "EOL";
          "AXESON"; "EOL";
          "PLOTON"; "0"; "."; "0"; "0"; "5"; ","; "0"; "."; "5"; "EOL"; "DISP";

          "CLS"; "EOL";
          "VIEWWINDOW"; "MINUS"; "5"; ","; "2"; ","; "0"; "."; "5"; ","; "MINUS"; "2"; ","; "8"; ","; "0"; "."; "5"; "EOL";
          "TEXT"; "3"; "0"; ","; "1"; "0"; ","; "QUOTE"; "MINUS"; "5"; ","; "2"; ","; "0"; "."; "5"; ","; "MINUS"; "2"; ","; "8"; ","; "0"; "."; "5"; "QUOTE"; "EOL";
          "PLOTON"; "0"; "."; "1"; ","; "1"; "EOL";
          "PLOTON"; "0"; "."; "1"; ","; "MINUS"; "1"; "DISP";
          (* These PlotOns are supposed to be one pixel to the right of the scales,
            here they are two pixels to the right.
            This is caused by the imprecision of floating number calculations is my rescale function. *)

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
  in
  run_test prog;;

let run_drawstat () =
  let prog =
    compile_test
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
  in
  run_test prog;;

let run_bgpict () =
  let prog =
    compile_test
      [("main",
        [ (* Checking RclPict, RclCapt, returning to the gscreen after the display of the capture *)
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
      m.(5).(i) <- true;
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
  run_with_projcont p prog;;

let run_frame () =
  let prog =
    compile_test
      [("main",
        [ (* Checking that the texts do not erase the frame and are displayed correctly *)
          "VIEWWINDOW"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "1"; ","; "6"; "3"; ","; "0"; "EOL";
          "TEXT"; "5"; "8"; ","; "0"; "2"; "5"; ","; "QUOTE"; "A"; "B"; "C"; "QUOTE"; "EOL";
          "VERTICAL"; "3" ; "6"; "EOL";
          "HORIZONTAL"; "6"; "3"; "DISP";
          "TEXT"; "5"; "8"; ","; "2"; "5"; ","; "QUOTE"; "A"; "B"; "C"; "QUOTE"; "EOL";
          "TEXT"; "5"; "8"; ","; "1"; "2"; "4"; ","; "QUOTE"; "A"; "B"; "C"; "QUOTE"; "EOL";
        ]
      )]
  in
  run_test prog;;

let run_style () =
  let prog =
    compile_test
      [("main",
        [ (* Checking that each F-line is pixel perfect compared with Casio Basic *)
          "VIEWWINDOW"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "1"; ","; "6"; "3"; ","; "0"; "EOL";
          "SLNORMAL"; "EOL";
          "FLINE"; "3"; "0"; ","; "8"; ","; "3"; "2"; ","; "1"; "1"; "EOL";
          "SKETCHDOT"; "FLINE"; "3"; "4"; ","; "8"; ","; "3"; "6"; ","; "1"; "1"; "EOL";

          "FLINE"; "3"; ","; "1"; "0"; ","; "1"; ","; "2"; "EOL";

          "PLOTON"; "1"; "0"; ","; "1"; "0"; "EOL";
          "FLINE"; "1"; "0"; ","; "8"; ","; "1"; "6"; ","; "8"; "EOL";
          "SKETCHDOT"; "FLINE"; "1"; "0"; ","; "5"; ","; "1"; "6"; ","; "5"; "EOL";
          "SKETCHDOT"; "FLINE"; "1"; "0"; ","; "3"; ","; "1"; "7"; ","; "3"; "EOL";

          "FLINE"; "1"; "9"; ","; "8"; ","; "2"; "3"; ","; "8"; "EOL";
          "SKETCHDOT"; "FLINE"; "1"; "9"; ","; "5"; ","; "2"; "3"; ","; "5"; "EOL";
          "SKETCHDOT"; "FLINE"; "1"; "9"; ","; "3"; ","; "2"; "4"; ","; "3"; "EOL";

          "SKETCHTHICK"; "FLINE"; "7"; "0"; ","; "3"; "0"; ","; "9"; "0"; ","; "3"; "0"; "EOL";
          "SKETCHDOT";   "FLINE"; "7"; "0"; ","; "3"; "5"; ","; "9"; "0"; ","; "3"; "5"; "EOL";
                         "FLINE"; "7"; "0"; ","; "3"; "3"; ","; "9"; "0"; ","; "3"; "3"; "EOL";

          "SKETCHTHICK"; "FLINE"; "7"; "5"; ","; "3"; "8"; ","; "9"; "5"; ","; "4"; "5"; "EOL";
          "SKETCHDOT";   "FLINE"; "7"; "5"; ","; "4"; "4"; ","; "9"; "5"; ","; "5"; "1"; "EOL";
                         "FLINE"; "7"; "5"; ","; "4"; "2"; ","; "9"; "5"; ","; "4"; "9"; "EOL";

          "SKETCHDOT";   "FLINE"; "7"; "5"; ","; "5"; "0"; ","; "9"; "6"; ","; "5"; "7"; "EOL";
                         "FLINE"; "7"; "5"; ","; "4"; "8"; ","; "9"; "6"; ","; "5"; "5"; "EOL";

          "FLINE"; "1"; "0"; "0"; ","; "4"; "0"; ","; "1"; "1"; "6"; ","; "4"; "4"; "EOL";
          "FLINE"; "1"; "0"; "0"; ","; "3"; "8"; ","; "1"; "1"; "6"; ","; "3"; "4"; "EOL";
          "FLINE"; "1"; "0"; "0"; ","; "3"; "0"; ","; "1"; "0"; "8"; ","; "3"; "2"; "EOL";
          "FLINE"; "1"; "0"; "0"; ","; "2"; "8"; ","; "1"; "0"; "8"; ","; "2"; "6"; "EOL";
          "FLINE"; "1"; "0"; "0"; ","; "2"; "3"; ","; "1"; "0"; "8"; ","; "2"; "4"; "EOL";
          "FLINE"; "1"; "0"; "0"; ","; "2"; "1"; ","; "1"; "0"; "8"; ","; "2"; "0"; "EOL";

          "SKETCHDOT";   "FLINE"; "1"; "0"; "0"; ","; "1"; "9"; ","; "1"; "0"; "8"; ","; "1"; "8"; "EOL";
                         "FLINE"; "1"; "0"; "0"; ","; "2"; "1"; ","; "1"; "0"; "8"; ","; "2"; "0"; "EOL";

                         "FLINE"; "1"; "0"; "0"; ","; "1"; "6"; ","; "1"; "0"; "9"; ","; "1"; "5"; "EOL";
          "SKETCHDOT";   "FLINE"; "1"; "0"; "0"; ","; "1"; "4"; ","; "1"; "0"; "9"; ","; "1"; "3"; "EOL";

          "SKETCHTHICK"; "FLINE"; "6"; "9"; ","; "3"; "8"; ","; "7"; "8"; ","; "6"; "2"; "EOL";
          "SKETCHDOT";   "FLINE"; "6"; "2"; ","; "3"; "8"; ","; "7"; "1"; ","; "6"; "2"; "EOL";
                         "FLINE"; "6"; "5"; ","; "3"; "8"; ","; "7"; "4"; ","; "6"; "2"; "EOL";

          "SKETCHTHICK"; "FLINE"; "6"; "0"; ","; "3"; "8"; ","; "6"; "0"; ","; "5"; "8"; "EOL";
          "SKETCHDOT";   "FLINE"; "5"; "6"; ","; "3"; "8"; ","; "5"; "6"; ","; "5"; "8"; "EOL";
                         "FLINE"; "5"; "7"; ","; "3"; "8"; ","; "5"; "7"; ","; "5"; "8"; "EOL";

          "SKETCHDOT";   "FLINE"; "5"; "1"; ","; "5"; "0"; ","; "5"; "1"; ","; "5"; "9"; "EOL";
                         "FLINE"; "5"; "3"; ","; "5"; "0"; ","; "5"; "3"; ","; "5"; "9"; "EOL";

          "SKETCHTHICK"; "FLINE"; "4"; "0"; ","; "3"; "8"; ","; "3"; "4"; ","; "5"; "8"; "EOL";
          "SKETCHDOT";   "FLINE"; "4"; "7"; ","; "3"; "8"; ","; "4"; "1"; ","; "5"; "8"; "EOL";
                         "FLINE"; "4"; "4"; ","; "3"; "8"; ","; "3"; "8"; ","; "5"; "8"; "EOL";

          "SKETCHDOT";   "FLINE"; "5"; "0"; ","; "3"; "8"; ","; "4"; "4"; ","; "5"; "9"; "EOL";
                         "FLINE"; "5"; "3"; ","; "3"; "8"; ","; "4"; "7"; ","; "5"; "9"; "EOL";

          "SKETCHTHICK"; "FLINE"; "3"; "2"; ","; "3"; "3"; ","; "1"; "2"; ","; "4"; "3"; "EOL";
          "SKETCHDOT";   "FLINE"; "3"; "2"; ","; "4"; "0"; ","; "1"; "2"; ","; "5"; "0"; "EOL";
                         "FLINE"; "3"; "2"; ","; "3"; "7"; ","; "1"; "2"; ","; "4"; "7"; "EOL";

          "SKETCHDOT";   "FLINE"; "2"; "2"; ","; "3"; "3"; ","; "1"; ","; "4"; "3"; "EOL";
                         "FLINE"; "2"; "2"; ","; "3"; "0"; ","; "1"; ","; "4"; "0"; "EOL";
          
          "FLINE"; "2"; "2"; ","; "5"; "4"; ","; "6"; ","; "5"; "7"; "EOL";
          "FLINE"; "2"; "4"; ","; "6"; "0"; ","; "1"; "6"; ","; "6"; "2"; "EOL";

          "SKETCHTHICK"; "FLINE"; "3"; "5"; ","; "2"; "7"; ","; "1"; "5"; ","; "2"; "7"; "EOL";
          "SKETCHDOT";   "FLINE"; "3"; "5"; ","; "2"; "3"; ","; "1"; "5"; ","; "2"; "3"; "EOL";
                         "FLINE"; "3"; "5"; ","; "2"; "5"; ","; "1"; "5"; ","; "2"; "5"; "EOL";

          "SKETCHTHICK"; "FLINE"; "2"; "5"; ","; "2"; "1"; ","; "1"; ","; "1"; "4"; "EOL";
          "SKETCHDOT";   "FLINE"; "2"; "5"; ","; "1"; "8"; ","; "1"; ","; "1"; "1"; "EOL";
                         "FLINE"; "2"; "5"; ","; "1"; "6"; ","; "1"; ","; "9"; "EOL";

          "SKETCHTHICK"; "FLINE"; "4"; "5"; ","; "3"; "0"; ","; "3"; "9"; ","; "1"; "0"; "EOL";
          "SKETCHDOT";   "FLINE"; "5"; "3"; ","; "3"; "0"; ","; "4"; "7"; ","; "1"; "0"; "EOL";
                         "FLINE"; "4"; "9"; ","; "3"; "0"; ","; "4"; "3"; ","; "1"; "0"; "EOL";

          "SKETCHTHICK"; "FLINE"; "5"; "6"; ","; "2"; "5"; ","; "5"; "6"; ","; "1"; "0"; "EOL";
          "SKETCHDOT";   "FLINE"; "5"; "8"; ","; "2"; "5"; ","; "5"; "8"; ","; "1"; "0"; "EOL";
                         "FLINE"; "6"; "0"; ","; "2"; "5"; ","; "6"; "0"; ","; "1"; "0"; "EOL";

          "SKETCHDOT";   "FLINE"; "5"; "5"; ","; "1"; ","; "5"; "5"; ","; "8"; "EOL";
                         "FLINE"; "5"; "7"; ","; "1"; ","; "5"; "7"; ","; "8"; "EOL";

          "SKETCHTHICK"; "FLINE"; "6"; "3"; ","; "2"; "5"; ","; "6"; "8"; ","; "1"; "0"; "EOL";
          "SKETCHDOT";   "FLINE"; "7"; "0"; ","; "2"; "5"; ","; "7"; "5"; ","; "1"; "0"; "EOL";
                         "FLINE"; "6"; "7"; ","; "2"; "5"; ","; "7"; "2"; ","; "1"; "0"; "EOL";

          "SKETCHTHICK"; "FLINE"; "8"; "0"; ","; "1"; "2"; ","; "1"; "1"; "0"; ","; "2"; "EOL";
          "SKETCHDOT";   "FLINE"; "9"; "1"; ","; "1"; "2"; ","; "1"; "2"; "1"; ","; "2"; "EOL";
                         "FLINE"; "9"; "6"; ","; "1"; "2"; ","; "1"; "2"; "6"; ","; "2"; "EOL";

          "FLINE"; "1"; "0"; "0"; ","; "5"; "0"; ","; "1"; "0"; "6"; ","; "5"; "7"; "EOL";
          "FLINE"; "1"; "2"; "0"; ","; "6"; "2"; ","; "1"; "1"; "5"; ","; "4"; "8"; "EOL";
          "FLINE"; "1"; "2"; "1"; ","; "6"; "2"; ","; "1"; "2"; "6"; ","; "4"; "8"; "DISP";
        ]
      )]
  in
  run_test prog;;



let run_broken () =
  let prog =
    compile_test
      [("main",
        [ (* Checking the Broken style *)
          "VIEWWINDOW"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "1"; ","; "6"; "3"; ","; "0"; "EOL";
          "SLNORMAL"; "EOL";
          "FLINE"; "3"; "0"; ","; "8"; ","; "3"; "2"; ","; "1"; "1"; "EOL";
          "SKETCHBROKEN"; "FLINE"; "3"; "4"; ","; "8"; ","; "3"; "6"; ","; "1"; "1"; "EOL";

          "FLINE"; "3"; "7"; ","; "8"; ","; "4"; "0"; ","; "1"; "3"; "EOL";
          "SKETCHBROKEN"; "FLINE"; "4"; "0"; ","; "8"; ","; "4"; "3"; ","; "1"; "3"; "EOL";
          
          "FLINE"; "1"; "0"; ","; "8"; ","; "1"; "6"; ","; "8"; "EOL";
          "SKETCHBROKEN"; "FLINE"; "1"; "0"; ","; "5"; ","; "1"; "6"; ","; "5"; "EOL";
          "SKETCHBROKEN"; "FLINE"; "1"; "0"; ","; "3"; ","; "1"; "7"; ","; "3"; "EOL";

          "FLINE"; "1"; "9"; ","; "8"; ","; "2"; "3"; ","; "8"; "EOL";
          "SKETCHBROKEN"; "FLINE"; "1"; "9"; ","; "5"; ","; "2"; "3"; ","; "5"; "EOL";
          "SKETCHBROKEN"; "FLINE"; "1"; "9"; ","; "3"; ","; "2"; "4"; ","; "3"; "EOL";

          "SKETCHBROKEN"; "FLINE"; "7"; "0"; ","; "3"; "5"; ","; "9"; "0"; ","; "3"; "5"; "EOL";
                          "FLINE"; "7"; "0"; ","; "3"; "3"; ","; "9"; "0"; ","; "3"; "3"; "EOL";

          "SKETCHBROKEN";   "FLINE"; "7"; "5"; ","; "4"; "4"; ","; "9"; "5"; ","; "5"; "1"; "EOL";
                         "FLINE"; "7"; "5"; ","; "4"; "2"; ","; "9"; "5"; ","; "4"; "9"; "EOL";

          "SKETCHBROKEN";   "FLINE"; "7"; "5"; ","; "5"; "0"; ","; "9"; "6"; ","; "5"; "7"; "EOL";
                         "FLINE"; "7"; "5"; ","; "4"; "8"; ","; "9"; "6"; ","; "5"; "5"; "EOL";

          "SKETCHBROKEN";   "FLINE"; "1"; "0"; "0"; ","; "1"; "9"; ","; "1"; "0"; "8"; ","; "1"; "8"; "EOL";
                            "FLINE"; "1"; "0"; "0"; ","; "2"; "1"; ","; "1"; "0"; "8"; ","; "2"; "0"; "EOL";

                            "FLINE"; "1"; "0"; "0"; ","; "1"; "6"; ","; "1"; "0"; "9"; ","; "1"; "5"; "EOL";
          "SKETCHBROKEN";   "FLINE"; "1"; "0"; "0"; ","; "1"; "4"; ","; "1"; "0"; "9"; ","; "1"; "3"; "EOL";

          "SKETCHBROKEN";   "FLINE"; "6"; "2"; ","; "3"; "7"; ","; "7"; "1"; ","; "6"; "3"; "EOL";
                         "FLINE"; "6"; "5"; ","; "3"; "7"; ","; "7"; "4"; ","; "6"; "3"; "EOL";

          "SKETCHBROKEN";   "FLINE"; "5"; "6"; ","; "3"; "8"; ","; "5"; "6"; ","; "5"; "8"; "EOL";
                         "FLINE"; "5"; "7"; ","; "3"; "8"; ","; "5"; "7"; ","; "5"; "8"; "EOL";

          "SKETCHBROKEN";   "FLINE"; "5"; "1"; ","; "5"; "0"; ","; "5"; "1"; ","; "5"; "9"; "EOL";
                         "FLINE"; "5"; "3"; ","; "5"; "0"; ","; "5"; "3"; ","; "5"; "9"; "EOL";

          "SKETCHBROKEN";   "FLINE"; "3"; "6"; ","; "3"; "8"; ","; "2"; "8"; ","; "5"; "8"; "EOL";

          "SKETCHBROKEN";   "FLINE"; "4"; "7"; ","; "3"; "8"; ","; "4"; "1"; ","; "5"; "8"; "EOL";
                         "FLINE"; "4"; "4"; ","; "3"; "8"; ","; "3"; "8"; ","; "5"; "8"; "EOL";

          "SKETCHBROKEN";   "FLINE"; "5"; "0"; ","; "3"; "8"; ","; "4"; "4"; ","; "5"; "9"; "EOL";
                         "FLINE"; "5"; "3"; ","; "3"; "8"; ","; "4"; "7"; ","; "5"; "9"; "EOL";

          "SKETCHBROKEN";   "FLINE"; "3"; "2"; ","; "4"; "0"; ","; "1"; "2"; ","; "5"; "0"; "EOL";
                         "FLINE"; "3"; "2"; ","; "3"; "7"; ","; "1"; "2"; ","; "4"; "7"; "EOL";

          "SKETCHBROKEN";   "FLINE"; "2"; "3"; ","; "3"; "3"; ","; "1"; ","; "4"; "3"; "EOL";
                         "FLINE"; "2"; "3"; ","; "3"; "0"; ","; "1"; ","; "4"; "0"; "EOL";
          
          "SKETCHBROKEN";   "FLINE"; "3"; "5"; ","; "2"; "3"; ","; "1"; "5"; ","; "2"; "3"; "EOL";
                         "FLINE"; "3"; "5"; ","; "2"; "5"; ","; "1"; "5"; ","; "2"; "5"; "EOL";

          "SKETCHBROKEN";   "FLINE"; "2"; "6"; ","; "1"; "8"; ","; "1"; ","; "1"; "1"; "EOL";
                         "FLINE"; "2"; "6"; ","; "1"; "6"; ","; "1"; ","; "9"; "EOL";

          "SKETCHBROKEN";   "FLINE"; "5"; "3"; ","; "3"; "0"; ","; "4"; "7"; ","; "1"; "0"; "EOL";
                         "FLINE"; "4"; "9"; ","; "3"; "0"; ","; "4"; "3"; ","; "1"; "0"; "EOL";

          "SKETCHBROKEN";   "FLINE"; "5"; "8"; ","; "2"; "6"; ","; "5"; "8"; ","; "1"; "0"; "EOL";
                         "FLINE"; "6"; "0"; ","; "2"; "6"; ","; "6"; "0"; ","; "1"; "0"; "EOL";

          "SKETCHBROKEN";   "FLINE"; "5"; "5"; ","; "1"; ","; "5"; "5"; ","; "8"; "EOL";
                         "FLINE"; "5"; "7"; ","; "1"; ","; "5"; "7"; ","; "8"; "EOL";

          "SKETCHBROKEN";   "FLINE"; "7"; "0"; ","; "2"; "5"; ","; "7"; "5"; ","; "0"; "9"; "EOL";
                         "FLINE"; "6"; "7"; ","; "2"; "5"; ","; "7"; "2"; ","; "0"; "9"; "EOL";

          "SKETCHBROKEN";   "FLINE"; "9"; "1"; ","; "1"; "2"; ","; "1"; "2"; "1"; ","; "2"; "EOL";
                         "FLINE"; "9"; "6"; ","; "1"; "2"; ","; "1"; "2"; "6"; ","; "2"; "EOL";

          "FLINE"; "1"; "1"; "0"; ","; "4"; "5"; ","; "1"; "2"; "5"; ","; "6"; "0"; "EOL";
          "SKETCHBROKEN"; "FLINE"; "1"; "0"; "7"; ","; "4"; "5"; ","; "1"; "2"; "2"; ","; "6"; "0"; "DISP";
        ]
      )]
  in
  run_test prog;;


let run_broken2 () =
  let prog =
    compile_test
      [("main",
        [ (* Checking the Broken style *)
          "VIEWWINDOW"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "1"; ","; "6"; "3"; ","; "0"; "EOL";
          "SLNORMAL"; "EOL";
          
          "FLINE"; "1"; "0"; ","; "3"; "5"; ","; "2"; "0"; ","; "3"; "5"; "EOL";
          "SKETCHBROKEN"; "FLINE"; "1"; "0"; ","; "3"; "2"; ","; "2"; "0"; ","; "3"; "2"; "EOL";

          "SKETCHTHICK"; "FLINE"; "2"; "0"; ","; "2"; "8"; ","; "2"; "0"; ","; "2"; "8"; "EOL";
          "SKETCHTHICK"; "FLINE"; "1"; "7"; ","; "2"; "8"; ","; "1"; "7"; ","; "2"; "8"; "EOL";
          "SKETCHTHICK"; "FLINE"; "1"; "4"; ","; "2"; "8"; ","; "1"; "4"; ","; "2"; "8"; "EOL";

          "FLINE"; "1"; "0"; ","; "1"; "8"; ","; "2"; "1"; ","; "1"; "8"; "EOL";
          "SKETCHBROKEN"; "FLINE"; "1"; "0"; ","; "1"; "5"; ","; "2"; "1"; ","; "1"; "5"; "EOL";

          "FLINE"; "1"; "0"; ","; "1"; "0"; ","; "2"; "2"; ","; "1"; "0"; "EOL";
          "SKETCHBROKEN"; "FLINE"; "1"; "0"; ","; "6"; ","; "2"; "2"; ","; "6"; "DISP";
        ])]
  in
  run_test prog;;

let run_diagonal () =
  let prog =
    compile_test
      [("main",
        [ (* Checking the diagonals in Dot and Broken styles *)
          "VIEWWINDOW"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "1"; ","; "6"; "3"; ","; "0"; "EOL";
          "SLNORMAL"; "EOL";
          
          "FLINE"; "6"; "5"; ","; "3"; "5"; ","; "8"; "2"; ","; "5"; "2"; "EOL";
          "SKETCHDOT"; "FLINE"; "6"; "8"; ","; "3"; "5"; ","; "8"; "5"; ","; "5"; "2"; "EOL";
          "SKETCHBROKEN"; "FLINE"; "7"; "3"; ","; "3"; "5"; ","; "9"; "0"; ","; "5"; "2"; "EOL";

          "FLINE"; "6"; "0"; ","; "3"; "5"; ","; "4"; "3"; ","; "5"; "2"; "EOL";
          "SKETCHDOT"; "FLINE"; "5"; "7"; ","; "3"; "5"; ","; "4"; "0"; ","; "5"; "2"; "EOL";
          "SKETCHBROKEN"; "FLINE"; "5"; "4"; ","; "3"; "5"; ","; "3"; "7"; ","; "5"; "2"; "EOL";

          "FLINE"; "6"; "0"; ","; "3"; "0"; ","; "4"; "3"; ","; "1"; "3"; "EOL";
          "SKETCHDOT"; "FLINE"; "5"; "7"; ","; "3"; "0"; ","; "4"; "0"; ","; "1"; "3"; "EOL";
          "SKETCHBROKEN"; "FLINE"; "5"; "4"; ","; "3"; "0"; ","; "3"; "7"; ","; "1"; "3"; "EOL";

          "FLINE"; "6"; "5"; ","; "3"; "0"; ","; "8"; "2"; ","; "1"; "3"; "EOL";
          "SKETCHDOT"; "FLINE"; "6"; "8"; ","; "3"; "0"; ","; "8"; "5"; ","; "1"; "3"; "EOL";
          "SKETCHBROKEN"; "FLINE"; "7"; "3"; ","; "3"; "0"; ","; "9"; "0"; ","; "1"; "3"; "DISP";
        ])]
  in
  run_test prog;;

let run_graph () =
  let prog =
    compile_test
      [("main",
        [ (* Checking GraphY=, GraphY>=, GraphY>... *)
          "AXESOFF"; "EOL";

          "VIEWWINDOW"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "5"; "9"; ","; "0"; ","; "0"; "EOL";
          "GRAPHS"; "1"; "1"; ","; "4"; "4"; ","; "1"; "2"; "6"; "DISP";

          "AXESON"; "EOL";

          "VIEWWINDOW"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "MINUS"; "6"; "0"; ","; "6"; "3"; ","; "0"; "EOL";
          "GRAPHS"; "2"; "0"; "COS"; "LPAR"; "X"; "DIVIDED"; "1"; "0"; "RPAR"; "PLUS"; "5"; "0"; ","; "5"; "0"; ","; "1"; "0"; "0"; "DISP";

          "VIEWWINDOW"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "6"; "3"; ","; "MINUS"; "6"; "0"; ","; "0"; "EOL";
          "GRAPHS"; "2"; "0"; "COS"; "LPAR"; "X"; "DIVIDED"; "1"; "0"; "RPAR"; "PLUS"; "5"; "0"; ","; "5"; "0"; ","; "1"; "0"; "0"; "DISP";
          
          "VIEWWINDOW"; "1"; "2"; "7"; ","; "1"; ","; "0"; ","; "1"; ","; "6"; "3"; ","; "0"; "EOL";
          "GRAPHXGEQ"; "0"; "."; "1"; "LPAR"; "Y"; "MINUS"; "1"; "5"; "RPAR"; "POWER2"; "DISP";
          "CLS"; "EOL";
          "GRAPHXLEQ"; "0"; "."; "1"; "LPAR"; "Y"; "MINUS"; "1"; "5"; "RPAR"; "POWER2"; "DISP";
          "CLS"; "EOL";
          "GRAPHXG"; "0"; "."; "1"; "LPAR"; "Y"; "MINUS"; "1"; "5"; "RPAR"; "POWER2"; "DISP";
          "CLS"; "EOL";
          "GRAPHXL"; "0"; "."; "1"; "LPAR"; "Y"; "MINUS"; "1"; "5"; "RPAR"; "POWER2"; "DISP";
          "CLS"; "EOL";
          "GRAPHXEQ"; "0"; "."; "1"; "LPAR"; "Y"; "MINUS"; "1"; "5"; "RPAR"; "POWER2"; "DISP";
          "CLS"; "EOL";

          "VIEWWINDOW"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "6"; "3"; ","; "1"; ","; "0"; "EOL";
          "GRAPHXGEQ"; "0"; "."; "1"; "LPAR"; "Y"; "MINUS"; "1"; "5"; "RPAR"; "POWER2"; "DISP";
          "CLS"; "EOL";
          "GRAPHXLEQ"; "0"; "."; "1"; "LPAR"; "Y"; "MINUS"; "1"; "5"; "RPAR"; "POWER2"; "DISP";
          "CLS"; "EOL";
          "GRAPHXG"; "0"; "."; "1"; "LPAR"; "Y"; "MINUS"; "1"; "5"; "RPAR"; "POWER2"; "DISP";
          "CLS"; "EOL";
          "GRAPHXL"; "0"; "."; "1"; "LPAR"; "Y"; "MINUS"; "1"; "5"; "RPAR"; "POWER2"; "DISP";
          "CLS"; "EOL";
          "GRAPHXEQ"; "0"; "."; "1"; "LPAR"; "Y"; "MINUS"; "1"; "5"; "RPAR"; "POWER2"; "DISP";
          "CLS"; "EOL";

          "VIEWWINDOW"; "MINUS"; "1"; "0"; ","; "5"; ","; "1"; ","; "MINUS"; "5"; ","; "2"; "."; "5"; ","; "1"; "EOL";
          "HORIZONTAL"; "2"; "DISP";
          
          "CLS"; "EOL";
          "GRAPHYEQ"; "MINUS"; "X"; "POWER2"; "PLUS"; "1"; "DISP";

          "CLS"; "EOL";
          "GRAPHYG"; "MINUS"; "X"; "POWER2"; "PLUS"; "1"; "DISP";
          "CLS"; "EOL";
          "GRAPHYGEQ"; "MINUS"; "X"; "POWER2"; "PLUS"; "1"; "DISP";

          "CLS"; "EOL";
          "GRAPHYL"; "MINUS"; "X"; "POWER2"; "PLUS"; "1"; "DISP";
          "CLS"; "EOL";
          "GRAPHYLEQ"; "MINUS"; "X"; "POWER2"; "PLUS"; "1"; "DISP";

          "VIEWWINDOW"; "MINUS"; "1"; "0"; ","; "5"; ","; "1"; ","; "2"; "."; "5"; ","; "MINUS"; "5"; ","; "1"; "EOL";
          "HORIZONTAL"; "2"; "DISP";
          
          "CLS"; "EOL";
          "GRAPHYEQ"; "MINUS"; "X"; "POWER2"; "PLUS"; "1"; "DISP";

          "CLS"; "EOL";
          "GRAPHYG"; "MINUS"; "X"; "POWER2"; "PLUS"; "1"; "DISP";
          "CLS"; "EOL";
          "GRAPHYGEQ"; "MINUS"; "X"; "POWER2"; "PLUS"; "1"; "DISP";

          "CLS"; "EOL";
          "GRAPHYL"; "MINUS"; "X"; "POWER2"; "PLUS"; "1"; "DISP";
          "CLS"; "EOL";
          "GRAPHYLEQ"; "MINUS"; "X"; "POWER2"; "PLUS"; "1"; "DISP";
        ])]
  in
  run_test prog;;


let run_timer () =
  let prog =
    compile_test
      [("main",
        [ (* Checking GraphY=, GraphY>=, GraphY>... *)
          "VIEWWINDOW"; "MINUS"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "1"; ","; "6"; "3"; ","; "0"; "EOL";
          "FOR"; "1"; "ASSIGN"; "A"; "TO"; "1"; "0"; "0"; "EOL";
          (* "FLINE"; "1"; "0"; ","; "2"; "0"; ","; "1"; "1"; "0"; ","; "5"; "0"; "EOL"; *)
          "TEXT"; "1"; "0"; ","; "1"; "0"; ",";
            "QUOTE"; "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K";
              "L"; "M"; "N"; "O"; "P"; "Q"; "R"; "S"; "T"; "U"; "V"; "W";
              "X"; "Y"; "Z"; "QUOTE"; "EOL";
          "NEXT"; "EOL";
          "QUOTE"; "A"; "QUOTE"; "EOL";
        ])]
  in
  run_test prog;;

(* Debug of Ace Combat level 9 *)
let run_truncate () =
  let prog =
    compile_test
      [("main",
        [
          "VIEWWINDOW"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "1"; ","; "6"; "3"; ","; "0"; "EOL";
          (* "3"; "ASSIGN"; "A"; "COLON"; "5"; "0"; "0"; "ASSIGN"; "H"; "EOL";
          "0"; "ASSIGN"; "LIST"; "1"; "LSQBRACKET"; "1"; "EOL";
          "0"; "ASSIGN"; "LIST"; "1"; "LSQBRACKET"; "2"; "EOL"; *)
          "SGPH1"; "DRAWON"; ","; "XYLINE"; ","; "LIST"; "3"; ","; "LIST"; "4"; ","; "1"; ","; "DOT"; "EOL";
          (* "SGPH2"; "DRAWON"; "XYLINE"; "LIST"; "5"; "LIST"; "6"; ","; "1"; ","; "DOT"; "EOL"; *)
          "SGPH2"; "DRAWOFF"; (* ","; "XYLINE"; ","; "LIST"; "1"; ","; "LIST"; "2"; ","; "1"; ","; "DOT"; *) "EOL";
          "SGPH3"; "DRAWOFF"; "EOL";
          "LBRACKET"; "3"; "8"; ","; "2"; "0"; ","; "2"; "0"; ","; "0"; ","; "0"; ",";
          "2"; "0"; ","; "2"; "0"; ","; "1"; "0"; "8"; ","; "1"; "0"; "8"; ",";
          "1"; "2"; "8"; ","; "1"; "2"; "8"; ","; "1"; "0"; "8"; ","; "1"; "0"; "8"; ","; "8"; "7"; "ASSIGN"; "LIST"; "3"; "EOL";
          "LBRACKET"; "9"; ","; "9"; ","; "4"; "0"; ","; "4"; "0"; ","; "3"; "0"; ","; "3"; "0"; ","; "5"; "5"; ",";
          "5"; "5"; ","; "3"; "0"; ","; "3"; "0"; ","; "4"; "0"; ","; "4"; "0"; ","; "9"; ","; "9"; "ASSIGN"; "LIST"; "4"; "EOL";
          (* "LBRACKET"; "2"; ","; "1"; "0"; ","; "2"; "0"; "ASSIGN"; "LIST"; "1"; "EOL";
          "LBRACKET"; "5"; ","; "5"; "0"; ","; "1"; "0"; "ASSIGN"; "LIST"; "2"; "EOL"; *)
          "DRAWSTAT"; "DISP";
        ])]
  in
  (* prog;; *)
  run_test prog;;


(* Test of erasing the black square in text and graphic modes *)
let run_black_square () =
  let prog =
    compile_test
      [("main",
        [
          "VIEWWINDOW"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "1"; ","; "6"; "3"; ","; "0"; "EOL";
          "BGPICT"; "1"; "EOL";
          "FLINE"; "1"; ","; "6"; "3"; ","; "1"; "2"; "7"; ","; "6"; "3"; "EOL";
          "FOR"; "1"; "ASSIGN"; "A"; "TO"; "1"; "0"; "0"; "0"; "EOL";
          "NEXT"; "DISP";
          "PLOTON"; "1"; ","; "1"; "DISP";

          "CLRTEXT"; "EOL";
          "QUOTE"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A";
          "A"; "A"; "A"; "A"; "A"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "QUOTE"; "EOL";
          "FOR"; "1"; "ASSIGN"; "A"; "TO"; "1"; "0"; "0"; "0"; "EOL";
          "NEXT"; "EOL";
          "QUOTE"; "C"; "QUOTE"; "DISP"
        ])]
  in
  let p =
    let par = empty_projcont () in
    let m = Array.make_matrix 64 128 false in
    for j = 0 to 63 do
      m.(j).(126) <- true;
      if j mod 2 = 0 then
        m.(j).(127) <- true
    done;
    par.pict.(0) <- (2048, m);
    par
  in
  run_with_projcont p prog;;


(* Debug Break *)
let run_break () =
  let prog =
    compile_test
      [("main",
        [
          "1"; "ASSIGN"; "X"; "EOL";
          "WHILE"; "X"; "DIFFERENT"; "1"; "0"; "EOL";
          "X"; "PLUS"; "1"; "ASSIGN"; "X"; "EOL";
          "LOCATE"; "3"; ","; "3"; ","; "X"; "EOL";
          "IF"; "X"; "EQUAL"; "5"; "COLON"; "THEN"; "BREAK"; "EOL";
          "IFEND"; "EOL";
          "WHILEEND";
          "X"; "DISP";
        ])]
  in
  run_test prog;;


(* Test of erasing the black square in text and graphic modes *)
let run_stopict () =
  let prog =
    compile_test
      [("main",
        [
          "VIEWWINDOW"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "1"; ","; "6"; "3"; ","; "0"; "EOL";
          "BGPICT"; "1"; "EOL";
          "CLS"; "DISP";
          "PLOTON"; "8"; "0"; ","; "2"; "0"; "DISP";

          "FLINE"; "1"; ","; "1"; "0"; ","; "3"; "0"; ","; "3"; "0"; "EOL";
          "FLINE"; "1"; ","; "3"; "0"; ","; "3"; "0"; ","; "1"; "0"; "EOL";
          "FLINE"; "1"; ","; "1"; "0"; ","; "3"; "0"; ","; "1"; "0"; "EOL";
          "FLINE"; "1"; ","; "3"; "0"; ","; "3"; "0"; ","; "3"; "0"; "EOL";
          "FLINE"; "1"; ","; "1"; "0"; ","; "1"; ","; "3"; "0"; "EOL";
          "FLINE"; "3"; "0"; ","; "1"; "0"; ","; "3"; "0"; ","; "3"; "0"; "EOL";

          "STOPICT"; "2"; "COLON";
          "BGPICT"; "2"; "EOL";

          "PLOTON"; "1"; "0"; "0"; ","; "2"; "0"; "DISP";

          "CLS";
          "TEXT"; "1"; "0"; ","; "2"; "0"; ","; "QUOTE"; "A"; "B"; "C"; "QUOTE"; "EOL";
          
          "CLRTEXT"; "EOL";
          
          "TEXT"; "1"; "0"; ","; "4"; "0"; ","; "QUOTE"; "A"; "B"; "C"; "QUOTE"; "EOL";

          "VERTICAL"; "6"; "0"; "EOL";
          "VERTICAL"; "6"; "1"; "EOL";
          "VERTICAL"; "6"; "2"; "DISP";

          (* "CLRTEXT"; "EOL";
          
          "HORIZONTAL"; "3"; "0"; "EOL";
          "HORIZONTAL"; "3"; "1"; "EOL";
          "HORIZONTAL"; "3"; "2"; "DISP"; *)
        ])]
  in
  let p =
    let par = empty_projcont () in
    let m = Array.make_matrix 64 128 false in
    for j = 0 to 63 do
      m.(j).(126) <- true;
      if j mod 2 = 0 then
        m.(j).(127) <- true
    done;
    par.pict.(0) <- (2048, m);
    par
  in
  run_with_projcont p prog;;


(* Debug Timeless Remix decompression *)
let run_dcmp () =
  let prog =
    compile_test
      [("main",
        [
          "8"; "0"; "6"; "6"; "0"; "0"; "2"; "5"; "3"; "1"; "4"; "2"; "7"; "9"; "PLUS";
          "1"; "5"; "2"; "6"; "4"; "0"; "4"; "0"; "6"; "0"; "0"; "6"; "9"; "1"; "2"; "CPLXI"; "ASSIGN"; "G"; "EOL";

          (* 
            Level 2:

            Actual value of G:
            80660025314279 + 152640406006912 i

            Values of G, H, Ans:
            8.066002531e+13 + 1.52640406e+14i
            8066
            10e+10

            25314279 + 1.52640406e+14i
            253
            100000

            14279 + 1.52640406e+14i
            14279
            1

            1.52640406e+14i
            15264
            10e+10

            406006912i
            4060
            100000

            6912i
            6912
            1
          *)
          
          "FOR"; "1"; "ASSIGN"; "X"; "TO"; "6"; "EOL";
          "G"; "DISP";
          "1"; "0"; "POWER"; "LPAR"; "1"; "5"; "MINUS"; "5"; "LPAR"; "1"; "PLUS"; "MOD"; "X"; "MINUS"; "1"; ","; "3"; "EOL";

          "X"; "LEQ"; "3"; "IMPL"; "INT"; "LPAR"; "REP"; "G"; "DIVIDED"; "ANS"; "ASSIGN"; "H"; "EOL";
          "X"; "GREATER"; "3"; "IMPL"; "INT"; "LPAR"; "IMP"; "G"; "DIVIDED"; "ANS"; "ASSIGN"; "H"; "EOL";

          "X"; "LEQ"; "3"; "IMPL"; "CPLXI"; "IMP"; "G"; "PLUS"; "ANS"; "FRAC"; "LPAR"; "REP"; "G"; "DIVIDED"; "ANS"; "ASSIGN"; "G"; "EOL";
          "X"; "GREATER"; "3"; "IMPL"; "ANS"; "FRAC"; "LPAR"; "CPLXI"; "IMP"; "G"; "DIVIDED"; "ANS"; "ASSIGN"; "G"; "EOL";
          "H"; "DISP";

          "0"; "ASSIGN"; "S"; "COLON"; "1"; "ASSIGN"; "M"; "EOL";
          "WHILE"; "H"; "EOL";
          "H"; "RMDR"; "2"; "ASSIGN"; "I"; "EOL";
          "H"; "INTDIV"; "2"; "ASSIGN"; "H"; "EOL";
          "I"; "M"; "PLUS"; "S"; "ASSIGN"; "S"; "EOL";
          "1"; "0"; "M"; "ASSIGN"; "M"; "EOL";
          "WHILEEND"; 

          (* "QUOTE"; "S"; "QUOTE"; "EOL"; *)
          "S"; "DISP";
          (* "QUOTE"; "M"; "QUOTE"; "EOL"; *)
          "M"; "DISP";

          "FOR"; "0"; "ASSIGN"; "Y"; "TO"; "1"; "3"; "EOL";

          "INT"; "LPAR"; "S"; "DIVIDED"; "1"; "0"; "POWER"; "LPAR"; "1"; "3"; "MINUS"; "Y"; "DISP";
          "2"; "PLUS"; "LPAR"; "1"; "4"; "LPAR"; "X"; "MINUS"; "1"; "RPAR"; "PLUS"; "Y"; "RPAR"; "INTDIV"; "1"; "9"; "DISP";
          "2"; "PLUS"; "LPAR"; "1"; "4"; "LPAR"; "X"; "MINUS"; "1"; "RPAR"; "PLUS"; "Y"; "RPAR"; "RMDR"; "1"; "9"; "DISP";
          "1"; "0"; "POWER"; "LPAR"; "1"; "3"; "MINUS"; "Y"; "RPAR"; "FRAC"; "LPAR"; "S"; "DIVIDED"; "1"; "0"; "POWER"; "LPAR"; "1"; "3"; "MINUS"; "Y"; "ASSIGN"; "S"; "EOL";

          (* "QUOTE"; "N"; "E"; "W"; " "; "S"; "QUOTE"; "EOL"; *)
          "S"; "DISP";

          "NEXT";

          "NEXT";
        ])]
  in
  run_test prog;;

(* Test for compressed pictures *)
let run_compr () =
  let prog =
    compile_test
      [("main",
        [
          "VIEWWINDOW"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "1"; ","; "6"; "3"; ","; "0"; "EOL";
          "RCLPICT"; "2"; "DISP";
          "CLS"; "EOL";
          "PLOTOFF"; "1"; ","; "1"; "EOL";
          "FOR"; "1"; "1"; "ASSIGN"; "A"; "TO"; "1"; "9"; "EOL";
          "CLS"; "EOL";
          "TEXT"; "2"; "0"; ","; "1"; "0"; "0"; ","; "A"; "EOL";
          "RCLPICT"; "A"; "DISP";
          "NEXT";
        ])]
  in
  let p =
    let s = file_to_string "/mnt/c/users/blanc/desktop/jeux_casio_alex1186/airwolf/airwolf.g1m" in
    let prj = g1m_reader s true in
    prj.pict.(18) <- prj.pict.(1);
    (* for p = 10 to 17 do
      let (_,m) = prj.pict.(p) in
      for j = 0 to 5 do
        m.(j).(p) <- true;
        (* for a = 0 to 63 do
          m.(0).(2*a) <- true;
          m.(5).(2*a) <- true
        done; *)
        (* m.(0).(0) <- true; *)
        (* m.(0).(127) <- true; *)
        (* m.(5).(0) <- true; *)
        (* m.(5).(127) <- true *)
      done
    done; *)
    prj
  in
  run_with_projcont p prog;;

(* Test for speed of Picture drawing *)
let run_speed_pict () =
  let prog =
    compile_test
      [("main",
        [
          (* "VIEWWINDOW"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "1"; ","; "6"; "3"; ","; "0"; "EOL"; *)
          "QUOTE"; "R"; "E"; "A"; "D"; "Y"; "?"; "QUOTE"; "DISP";
          (* "CLS"; "EOL";
          "FOR"; "1"; "ASSIGN"; "A"; "TO"; "2"; "0"; "0"; "EOL";
          "RCLPICT"; "1"; "EOL";
          "NEXT"; *)
          "QUOTE"; "D"; "O"; "N"; "E"; "QUOTE"; "DISP";
        ])]
  in
  let p =
    let s = file_to_string "/mnt/c/users/blanc/desktop/jeux_casio_alex1186/airwolf/airwolf.g1m" in
    let prj = g1m_reader s true in
    prj.pict.(18) <- prj.pict.(1);
    prj
  in
  run_with_projcont p prog;;


let run_circle () =
  let prog =
    compile_test
      [("main",
        [
          "VIEWWINDOW"; "UMINUS"; "2"; ","; "2"; ","; "0"; ","; "UMINUS"; "1"; ","; "1"; ","; "0"; "EOL";
          "CIRCLE"; "1"; ","; "0"; "."; "2"; ","; "0"; "."; "8"; "DISP";
          "CLS"; "EOL";
          "SKETCHDOT"; "CIRCLE"; "1"; ","; "0"; "."; "2"; ","; "0"; "."; "8"; "DISP";
          "CLS"; "EOL";
          "SKETCHTHICK"; "CIRCLE"; "1"; ","; "0"; "."; "2"; ","; "0"; "."; "8"; "DISP";
          "CLS"; "EOL";
          "SKETCHBROKEN"; "CIRCLE"; "1"; ","; "0"; "."; "2"; ","; "0"; "."; "8"; "DISP";
        ])]
  in
  run_test prog;;


(* Tests on BgPict update of the background *)
let run_bgpict2 () =
  let prog =
    compile_test
      [("main",
        [
          "VIEWWINDOW"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "1"; ","; "6"; "3"; ","; "0"; "EOL";

          "BGPICT"; "2"; "EOL";
          "STOPICT"; "5"; "COLON";
          "BGPICT"; "5"; "EOL";

          "PLOTON"; "2"; "0"; ","; "3"; "0"; "DISP";

          "BGPICT"; "1"; "EOL";
          "PLOTON"; "3"; "0"; ","; "3"; "0"; "DISP";

          "CLS"; "EOL";
          (* "VIEWWINDOW"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "1"; ","; "6"; "3"; ","; "0"; "EOL"; *)
          "BGPICT"; "2"; "EOL";
          "PLOTON"; "4"; "5"; ","; "3"; "0"; "DISP";

          "VIEWWINDOW"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "1"; ","; "6"; "3"; ","; "0"; "EOL";
          "BGPICT"; "2"; "EOL";
          "PLOTON"; "2"; "0"; ","; "3"; "0"; "DISP";
        ])]
  in
  let p =
    let par = empty_projcont () in
    let m1 = Array.make_matrix 64 128 false in
    for i = 20 to 100 do
      m1.(15).(i) <- true;
      m1.(63-15).(i) <- true
    done;
    par.pict.(0) <- (2048, m1);
    let m2 = Array.make_matrix 64 128 false in
    for i = 10 to 50 do
      m2.(i).(40) <- true;
      m2.(i).(80) <- true
    done;
    par.pict.(1) <- (2048, m2);
    par
  in
  run_with_projcont p prog;;


(* Test for menu command *)
let run_menu () =
  let prog =
    compile_test
      [("main",
        [
          "VIEWWINDOW"; "1"; ","; "1"; "2"; "7"; ","; "0"; ","; "1"; ","; "6"; "3"; ","; "0"; "EOL";
          "FLINE"; "1"; ","; "4"; "3"; ","; "1"; "2"; "7"; ","; "1"; "5"; "DISP";
          "3"; "ASSIGN"; "A"; "DISP";
          
          "LBL"; "E"; "EOL";

          "MENU"; "QUOTE"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9";
            "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9";
            "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "0";
            "QUOTE"; ",";
            "QUOTE"; "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K"; "L"; "M";
              "N"; "O"; "P"; "Q"; "R"; "S"; "T"; "U"; "V"; "W"; "X"; "Y"; "Z"; "QUOTE"; ","; "3"; ",";
            "QUOTE"; "B"; "B"; "B"; "QUOTE"; ","; "7"; ",";
            "QUOTE"; "C"; "C"; "C"; "QUOTE"; ","; "9"; ",";
            "QUOTE"; "D"; "D"; "D"; "QUOTE"; ","; "A"; ",";
            "QUOTE"; "E"; "E"; "E"; "QUOTE"; ","; "3"; ",";
            "QUOTE"; "F"; "F"; "F"; "QUOTE"; ","; "7"; ",";
            "QUOTE"; "G"; "G"; "G"; "QUOTE"; ","; "9"; ",";
            "QUOTE"; "H"; "H"; "H"; "QUOTE"; ","; "A"; ",";
            "QUOTE"; "I"; "I"; "I"; "QUOTE"; ","; "3"; "EOL";

          "LBL"; "3"; "COLON"; "3"; "DISP";
          "GOTO"; "E"; "EOL";

          "LBL"; "7"; "COLON"; "7"; "DISP";
          "GOTO"; "E"; "EOL";

          "LBL"; "9"; "COLON"; "9"; "DISP";
          "GOTO"; "E"; "EOL";

          "LBL"; "A"; "COLON"; "QUOTE"; "A"; "QUOTE"; "DISP";          
          "GOTO"; "E"; "EOL";
        ])]
  in
  (* prog;; *)
  run_test prog;;

(* Test for List "ABC" *)
let run_proglistzero () =
  let prog =
    (*
      "ABC" -> List 1[0]
      List 1[0] DISP    // Displays ABC
      "BCD" -> List "BCD"[0]
      List "BCD"[1]     // Displays 0
      2 -> List "BCD"[1]
      List "BCD"[1]     // Displays 2
      1 -> A
      List "BCD"[A]     // Displays 2
      0 -> A
      List "BCD"[A]     // Displays BCD
    *)
    compile_test
      [("main",
        [
          "QUOTE"; "A"; "B"; "C"; "QUOTE"; "ASSIGN"; "LIST"; "1"; "LSQBRACKET"; "0"; "EOL";
          "LIST"; "1"; "LSQBRACKET"; "0"; "DISP";
          (* "QUOTE"; "B"; "C"; "D"; "QUOTE"; "ASSIGN"; "LIST"; "QUOTE"; "B"; "C"; "D"; "QUOTE"; "LSQBRACKET"; "0"; "EOL";
          "LIST"; "QUOTE"; "B"; "C"; "D"; "QUOTE"; "LSQBRACKET"; "1"; "DISP";
          "2"; "ASSIGN"; "LIST"; "QUOTE"; "B"; "C"; "D"; "QUOTE"; "LSQBRACKET"; "1"; "DISP";
          "LIST"; "QUOTE"; "B"; "C"; "D"; "QUOTE"; "LSQBRACKET"; "1"; "DISP";
          "1"; "ASSIGN"; "A"; "EOL";
          "LIST"; "QUOTE"; "B"; "C"; "D"; "QUOTE"; "LSQBRACKET"; "A"; "DISP";
          "0"; "ASSIGN"; "A"; "EOL";
          "LIST"; "QUOTE"; "B"; "C"; "D"; "QUOTE"; "LSQBRACKET"; "A"; "DISP";
          *)
      ])]
  in
  (* prog;; *)
  run_test prog;;