(* Unit tests for Basic compilation *)

(* #use "basic_parsing/basic_compilation.ml"
#use "basic_parsing/basic_encoding.ml" *)

let prog1 () = compile 
  [("main",
  ["IF"; "1"; "EOL";  
  "THEN"; "QUOTE"; "A"; "QUOTE"; "DISP";
  "ELSE"; "QUOTE"; "B"; "QUOTE"; "DISP";
  "IFEND"; "EOL";
  "QUOTE"; "C"; "QUOTE"; "DISP"])];;

let prog2 () = compile 
  [("main",
  ["GOTO"; "E"; "EOL";
  "IF"; "1"; "EOL";
  "THEN";
    "LBL"; "E"; "EOL";
    "QUOTE"; "H"; "e"; "l"; "l"; "o"; "QUOTE"; "DISP";
  "ELSE";
    "QUOTE"; "B"; "y"; "e"; "QUOTE"; "DISP";
  "IFEND"; "EOL";
  "QUOTE"; "S"; "e"; "e"; " "; "y"; "o"; "u"; "!"; "QUOTE"; "DISP"])];;

(* Remark:
  When a Goto sends you inside a Then statement, the Else is NOT evaluated.
  The content of the Then is evaluated, then it jumps to after the IfEnd, no error is raised.
 *)

let prog3 () = compile
  [("main",
    ["FOR"; "3"; "MINUS"; "2"; "ASSIGN"; "A";
      "TO"; "8"; "STEP"; "2"; "EOL";
    "QMARK"; "ASSIGN"; "B"; "EOL";
    "B"; "TIMES"; "2"; "DISP";
    (* "LOCATE"; "1"; "0"; ","; "5"; ","; "QUOTE"; "A"; "B"; "C"; "QUOTE"; "DISP"; *)
    "NEXT";
    "QUOTE"; "T"; "H"; "E"; " "; "E"; "N"; "D"; "QUOTE"; "DISP"
    ]
  )];;

let prog4 () = compile
  [("MAIN",
    [
      "QUOTE"; "W"; "E"; "L"; "C"; "O"; "M"; "E"; " "; "T"; "O"; " "; "M"; "A"; "I"; "N"; "QUOTE"; "DISP";
      "PROG"; "QUOTE"; "A"; "U"; "X"; "1"; "QUOTE"; "EOL";
      "QUOTE"; "B"; "A"; "C"; "K"; " "; "T"; "O"; " "; "M"; "A"; "I"; "N"; "QUOTE"; "DISP";
      "PROG"; "QUOTE"; "A"; "U"; "X"; "2"; "QUOTE"; "EOL";
      "QUOTE"; "B"; "A"; "C"; "K"; " "; "T"; "O"; " "; "M"; "A"; "I"; "N"; "QUOTE"; "DISP";
      "QUOTE"; "B"; "Y"; "E"; "!"; "QUOTE"; "DISP"
    ]
  );
  
  ("AUX1",
    [
      "QUOTE"; "W"; "E"; "L"; "C"; "O"; "M"; "E"; " "; "T"; "O"; " "; "A"; "U"; "X"; "1"; "QUOTE"; "DISP";
      "QUOTE"; "S"; "E"; "E"; " "; "Y"; "O"; "U"; "!"; "QUOTE"; "DISP"
    ]
  );
  
  ("AUX2",
    [
      "QUOTE"; "W"; "E"; "L"; "C"; "O"; "M"; "E"; " "; "T"; "O"; " "; "A"; "U"; "X"; "2"; "QUOTE"; "DISP";
      "QUOTE"; "G"; "E"; "T"; " "; "O"; "U"; "T"; "!"; "QUOTE"; "DISP"
    ]
  )];;

(* Getkey *)
let prog_getkey () = compile
  [("main",
    [
      "WHILE"; "1"; "EOL";
      "GETKEY"; "ASSIGN"; "K"; "EOL";
      "IF"; "K"; "LEQ"; "9"; "EOL";
      "THEN"; "LOCATE"; "4"; ","; "3"; ","; "QUOTE"; " "; "QUOTE"; "EOL";
      "IFEND"; "EOL";
      "LOCATE"; "3"; ","; "3"; ","; "K"; "EOL";
      "WHILEEND"
    ]
  )];;

(* If, Else If, Else, IfEnd *)
let prog_if () = compile
  [("main",
    [
      "IF"; "0"; "COLON"; "THEN"; "QUOTE"; "A"; "QUOTE"; "DISP";
      "ELSE"; "IF"; "0"; "COLON"; "THEN"; "QUOTE"; "B"; "QUOTE"; "DISP";
      "ELSE"; "IF"; "0"; "COLON"; "THEN"; "QUOTE"; "C"; "QUOTE"; "DISP";
      "IFEND";
      "IFEND";
      "QUOTE"; "D"; "QUOTE"; "DISP";
      "IFEND";
      "QUOTE"; "E"; "QUOTE"; "DISP"
    ]
  )];;

(*
0 If (Arithm [Number (Value {Complex.re = 1.; im = 0.})], 4);
1 String ["A"];
2 Disp;
3 Goto 13;
4 If (Arithm [Number (Value {Complex.re = 2.; im = 0.})], 8);
5 String ["B"];
6 Disp;
7 Goto 11;
8 If (Arithm [Number (Value {Complex.re = 3.; im = 0.})], 11);
9 String ["C"];
10 Disp;
11 String ["D"];
12 Disp;
13 String ["E"];
14 Disp;
15 End
*)

(* Missing IfEnd (allowed in Casio Basic) *)
let prog_ifend () = compile
  [("main",
    [
      "IF"; "0"; "COLON"; "THEN"; "QUOTE"; "A"; "QUOTE"; "DISP";
      "ELSE"; "IF"; "0"; "COLON"; "THEN"; "QUOTE"; "B"; "QUOTE"; "DISP";
      "ELSE"; "IF"; "0"; "COLON"; "THEN"; "QUOTE"; "C"; "QUOTE"; "DISP";
    ]
  )];;

(*
0 If (Arithm [Number (Value {Complex.re = 0.; im = 0.})], 4);
1 String ["A"];
2 Disp;
3 Goto 11;
4 If (Arithm [Number (Value {Complex.re = 0.; im = 0.})], 8);
5 String ["B"];
6 Disp;
7 Goto 11;
8 If (Arithm [Number (Value {Complex.re = 0.; im = 0.})], 11);
9 String ["C"];
10 Disp;
11 End
*)