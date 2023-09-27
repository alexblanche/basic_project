(* Abstract Basic program type *)

(* Due to the presence of Lbl/Goto, a Casio Basic program cannot be represented as an
  abstract tree. *)
(* Each command is represented with an abstract command type.
  The program is compiled into an array of commands.
  - Patterns "Lbl A ... Goto A" are replaced with "Goto(l)",
  where l is the line where the Lbl A points to.
  - Conditionals (If) and loops (For, While, Do-LpWhile) are transformed with the Goto method. *)

  
(************************************************************************************)
(** Arithmetic expressions **)

(* Plan for integration of list/matrix arithmetic:
  Separate num_expr, list_expr and mat_expr at lexing time,
  each made up of a basic_expr containing some variables List _, Mat _, {...} and [[][]]
  
  The output type of the lexing function is the following:
  type expression_type = Numerical | ListExpr | MatExpr
  type expression = basic_expr * expression_type
  
  We define these specific types:
  type num_expr = basic_expr
  type list_expr = basic_expr
  type mat_expr = basic_expr

  basic_number now contains
  | Value of complex
  | Variable of variable
  | ListContent of num_expr array (* {...} *)
  | MatContent of num_expr array array (* [[...][...]...] *)
  | VarList of variable (* List _ *)
  | VarMat of int (* Mat _ *)
  
  Each constructor takes the specific type it needs, or a general expression
  Assign of num_expr * variable
  AssignList of list_expr * basic_number
  AssignMat of mat_expr * int

  There are three evaluation functions, one for each input (and output type)
  eval (num_expr -> complex)
  eval_list (list_expr -> complex array)
  eval_mat (mat_expr -> complex array array)

  At runtime, an error is raised if the sizes of two lists or two matrices do not match,
  or if unexpected types (MatContent or VarMat in a list_expr) are present in the same
  basic_expr (it should not happen if lexing is correct)
*)


(* entity, variable, arithm and basic_expr are circular recursive types *)

(* Type for arithmetic entities:
  Numbers and variables for numerical expressions
  ListContent ({1,2,3}) and VarList (List 1) for list expressions
  MatContent ([[1,2][3,4]]) and VarMat (Mat A) for matrix expressions *)
type entity =
  | Value of complex
  | Variable of variable
  | ListContent of num_expr array (* {...} *)
  | MatContent of num_expr array array (* [[...][...]...] *)
  | VarList of entity (* List _ (accepts Value or Variable) *)
  | VarMat of int (* Mat _ *)

and

(* Type for Basic variables *)
variable =
| Var of int (* index: 0..25 = A ... Z, 26 = r, 27 = theta, 28 = Ans *)
| ListIndex of entity * num_expr (* ListIndex(x, e) = List x[e] *)
| MatIndex of int * num_expr * num_expr (* MatIndex (x,e1,e2) -> Mat x[e1][e2] *)
| Getkey (* its value depends on the key currently pressed *)
| Random (* Ran# *)

and

(* Type for arithmetic expressions lexemes *)
arithm =
  | Entity of entity
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
  
(* Specific types for numerical expressions, list expressions and matrix expressions *)
and num_expr = basic_expr
and list_expr = basic_expr
and mat_expr = basic_expr

(* General expressions *)
type expression_type = Numerical | ListExpr | MatExpr
and expression = basic_expr * expression_type




(************************************************************************************)
(** Commands **)

(* Graphic screen functions *)
type graphic =
  | ViewWindow of num_expr * num_expr * num_expr * num_expr * num_expr * num_expr
  | PlotOn of num_expr * num_expr
  | PlotOff of num_expr * num_expr
  | Fline of num_expr * num_expr * num_expr * num_expr
  | GraphicText of num_expr * num_expr * (string list)
  | Graphic_Function of string (* Other graphic functions *)

(* Content of a Locate command *)
type locate_content =
  | Loc_text of string list
  | Loc_expr of num_expr

(* Type for Basic commands *)
type command =

  (* Delimitors *)
  | End (* End of the program *)
  | Empty (* Empty command, to complete the basic_code array *)

  (* Expressions, strings and text display *)
  | Expr of expression (* Arithmetic expression *)
  | String of string list (* Text, stored as list of lexemes (strings of 1 or 2 characters) *)
  | Locate of num_expr * num_expr * locate_content
    (* Locate (e1, e2, c): Locate function, prints c (string or result of an expression)
      at coordinates z1,z2 (where z1,z2 are the results of the evaluation of e1,e2) *)
  | Disp (* Displays the result of the line above *)
  
  (* -> *)
  | Assign of num_expr * variable (* expr -> X or expr -> List A[X] *)
  (* Lists accept variables: 3->A, {1} -> List A will assign {1} to List 3 *)
  | AssignList of list_expr * entity (* 2+3*{1,2,3} -> List A *)
  (* Matrices and Strings do not accept variables *)
  | AssignMat of mat_expr * int (* 5*[[1,2][3,4]] -> Mat A *)
  | AssignStr of string * int (* s -> Str i *)
  
  (* Jumps *)
  | Goto of int (* Jump to line l on the array *)
  | Prog of string (* Prog name: jump to program name, then come back *)
  | If of num_expr * int (* If (expr,l): If expr Then continue (else jump to line l) *)
  | JumpIf of num_expr * int (* JumpIf (expr,l): If expr Then jump to line l *)
  | For of int * num_expr * num_expr * num_expr * int
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
type basic_code = command array * (string * int) list;;
