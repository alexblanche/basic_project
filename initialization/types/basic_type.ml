(* Abstract Basic program type *)

(* Due to the presence of Lbl/Goto, a Casio Basic program cannot be represented as an
  abstract tree. *)
(* Each command is represented with an abstract command type.
  The program is compiled into an array of commands.
  - Patterns "Lbl A ... Goto A" are replaced with "Goto(l)",
  where l is the line where the Lbl A points to.
  - Conditionals (If) and loops (For, While, Do-LpWhile) are transformed with the Goto method. *)

(* variable, basic_number, arithm and basic_expr are circular recursive types *)

(* Type for Basic variables *)
type variable =
  | Var of int (* index: 0..25 = A ... Z, 26 = r, 27 = theta, 28 = Ans *)
  | ListIndex of basic_number * basic_expr (* ListIndex(x, e) = List x[e] *)
  | MatIndex of basic_number * basic_expr * basic_expr (* MatIndex (x,e1,e2) -> Mat x[e1][e2] *)
  | Getkey (* its value depends on the key currently pressed *)
  | Random (* Ran# *)

and

(* Type for Basic numbers *)
basic_number =
  | Value of complex
  | Variable of variable

and

(* Type for arithmetic expressions lexemes *)
arithm =
  | Number of basic_number
  | Lpar (* ( *) | Rpar (* ) *)
  | Op of string (* Binary operator *)
  | Runop of string (* Right unary operator *)
  | Lunop of string (* Left unary operator *)
  | Function of string * (basic_expr list) (* Function and list of arguments *)
  | Comma (* , *)

and

(* Type for Basic numerical and boolean expressions *)
(* Conditions are expressions: 0 = false, <>0 = true *)
basic_expr =
  | QMark (* ? (asks the user for a value) *)
  | Arithm of arithm list

(* Graphic screen functions *)
type graphic =
  | ViewWindow of basic_expr * basic_expr * basic_expr * basic_expr * basic_expr * basic_expr
  | PlotOn of basic_expr * basic_expr
  | PlotOff of basic_expr * basic_expr
  | Fline of basic_expr * basic_expr * basic_expr * basic_expr
  | GraphicText of basic_expr * basic_expr * (string list)
  | Graphic_Function of string (* Other graphic functions *)

(* Content of a Locate command *)
type locate_content =
  | Loc_text of string list
  | Loc_expr of basic_expr

(* Type for Basic commands *)
type command =

  (* Delimitors *)
  | End (* End of the program *)
  | Empty (* Empty command, to complete the basic_code array *)

  (* Expressions, strings and text display *)
  | Expr of basic_expr (* Arithmetic expression *)
  | String of string list (* Text, stored as list of lexemes (strings of 1 or 2 characters) *)
  | Locate of basic_expr * basic_expr * locate_content
    (* Locate (e1, e2, c): Locate function, prints c (string or result of an expression)
      at coordinates z1,z2 (where z1,z2 are the results of the evaluation of e1,e2) *)
  | Disp (* Displays the result of the line above *)
  
  (* -> *)
  | Assign of basic_expr * variable (* expr -> X or expr -> List A[X] *)
  (* Lists accept variables: 3->A, {1} -> List A will assign {1} to List 3 *)
  | AssignList of basic_expr * basic_number (* 2+3*{1,2,3} -> List A *) (* to be changed with expression (basic_expr * ListExpr) *)
  (* Matrices and Strings do not accept variables *)
  | AssignMat of basic_expr * int (* 5*[[1,2][3,4]] -> Mat A *) (* to be changed with expression (basic_expr * MatExpr) *) 
  | AssignStr of string * int (* s -> Str i *)
  
  (* Jumps *)
  | Goto of int (* Jump to line l on the array *)
  | Prog of string (* Prog name: jump to program name, then come back *)
  | If of basic_expr * int (* If (expr,l): If expr Then continue (else jump to line l) *)
  | JumpIf of basic_expr * int (* JumpIf (expr,l): If expr Then jump to line l *)
  | For of int * basic_expr * basic_expr * basic_expr * int
    (* For (vi,e1,e2,e3,i): For e1->V To e2 Step e3 (if not, jump to line i)
      Where V is the variable of index vi *)
  | Next (* Closes a For loop *)

  (* Graphic functions and other functions *)
  | Graphic of graphic (* Graphic screen functions *)
  | Function of string (* Any other function, stored in a hashtable *)

(* Type for compiled Basic code *)
(* An object of type basic_code has the form (code, progindex),
  where code is the compiled code of all the programs (separated by End)
  and progindex is a list of the name and index (in the code) of each program *)
type basic_code = (command array * (string * int) list);;
