(* Abstract Basic program type *)

(* Due to the presence of Lbl/Goto, a Casio Basic program cannot be represented as an
  abstract tree. *)
(* Each command is represented with an abstract command type.
  The program is compiled into an array of commands.
  - Patterns "Lbl A ... Goto A" are replaced with "Goto(l)",
  where l is the line where the Lbl A points to.
  - Conditionals (If) and loops (For, While, Do-LpWhile) are transformed with the Goto method. *)

(* Graphic screen functions *)
type graphic =
  | ViewWindow of num_expr * num_expr * num_expr * num_expr * num_expr * num_expr
  | PlotOn of num_expr * num_expr
  | PlotOff of num_expr * num_expr
  | Fline of num_expr * num_expr * num_expr * num_expr
  | GraphicText of num_expr * num_expr * (string list)
  | Graphic_Function of string (* Other graphic functions *)

(* String expressions *)
type string_expr =
  | Num_expr of num_expr (* numerical expression *)
  | Str_content of string list (* explicit definition of a string *)
  | Str_access of int (* Str i *)
  | Str_Func of string * string_expr list (* Function applied to several string_expr objects *)

(* Type for Basic commands *)
type command =

  (* Delimitors *)
  | End (* End of the program *)
  | Empty (* Empty command, to complete the basic_code array *)

  (* Expressions, strings and text display *)
  | Expr of basic_expr * expression_type (* Arithmetic expression *)
  | String of string list (* Text, stored as list of lexemes (strings of 1 or 2 characters) *)
  | Locate of num_expr * num_expr * string_expr
    (* Locate (e1, e2, c): Locate function, prints c (string or result of an expression)
      at coordinates z1,z2 (where z1,z2 are the results of the evaluation of e1,e2) *)
  | Disp (* Displays the result of the line above *)
  
  (* -> *)
  | Assign of num_expr * variable (* expr -> X or expr -> List A[X] *)
  | AssignMult of num_expr * int * int (* expr -> X~Z *)
  (* Lists accept variables: 3->A, {1} -> List A will assign {1} to List 3 *)
  | AssignList of list_expr * entity (* 2+3*{1,2,3} -> List A *)
  (* Matrices and Strings do not accept variables *)
  | AssignMat of mat_expr * int (* 5*[[1,2][3,4]] -> Mat A *)
  | AssignStr of string list * int (* s -> Str i *)
  
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
  | Function of string (* Any other function *)

(* Type for compiled Basic code *)
(* An object of type basic_code has the form (code, progindex),
  where code is the compiled code of all the programs (separated by End)
  and progindex is a list of the name and index (in the code) of each program *)
type basic_code = command array * (string * int) list;;
