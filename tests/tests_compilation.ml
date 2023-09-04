(* Unit tests for Basic compilation *)

(* #use "basic_parsing/basic_compilation.ml"
#use "basic_parsing/basic_encoding.ml" *)

let prog1 = compile 
  [("main",
  ["IF"; "1"; "EOL";  
  "THEN"; "QUOTE"; "A"; "QUOTE"; "DISP";
  "ELSE"; "QUOTE"; "B"; "QUOTE"; "DISP";
  "IFEND"; "EOL";
  "QUOTE"; "C"; "QUOTE"; "DISP"])];;

let prog2 = compile 
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

(* Question:
  When a Goto sends you inside a Then statement, the Else is NOT evaluated.
  The content of the Then is evaluated, then it jumps to after the IfEnd, no error is raised.
 *)