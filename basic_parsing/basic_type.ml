(* Abstract Basic program type *)

(* Due to the presence of Lbl/Goto, a Casio Basic program cannot be represented as an
  abstract tree. *)
(* Each command is represented with an abstract command type.
  The program is compiled into an array of commands.
  - Patterns "Lbl A ... Goto A" are replaced with "Goto(l)",
  where l is the line where the Lbl A points to.
  - Conditionals (If) and loops (For, While, Do-LpWhile) are transformed with the Goto method. *)

(* Lists, Matrices, Strings *)
type data_struct =
  | List of int
  | Mat of char
  | Str of int

(* Type for functions, of arity 1 to 4 *)
(* Greater arity is not needed for Casio Basic *)
type funct =
  | LOP of (float -> float)
  | AR1 of (float -> float)
  | AR2 of (float -> float -> float)
  | AR3 of (float -> float -> float -> float)
  | AR4 of (float -> float -> float -> float -> float)

(* variable, basic_number, arithm and basic_expr are circular recursive types *)

(* Type for Basic variables *)
type variable =
  | Var of int (* index: 0..25 = A ... Z, 26 = r, 27 = theta, 28 = Ans *)
  | ListIndex of data_struct * basic_expr (* ListIndex(List(x), e) = List x[e] *)
  | MatIndex of data_struct * basic_expr * basic_expr (* MatIndex (Mat(x),e1,e2) -> Mat x[e1][e2] *)
  | Getkey (* its value depends on the key currently pressed *)
  | Random (* Ran# *)

and

(* Type for Basic numbers *)
basic_number =
  | Float of float
  | Variable of variable

and

(* Type for arithmetic expressions lexemes *)
arithm =
  | Number of basic_number
  | Lpar (* ( *) | Rpar (* ) *)
  | Op of string (* Binary operator *)
  | Runop of string (* Right unary operator *)
  | Lunop of string (* Left unary operator *)
  | Function of string
  | Comma (* , *)

and

(* Type for Basic numerical and boolean expressions *)
(* Conditions are expressions: 0 = false, <>0 = true *)
basic_expr =
  | QMark (* ? (asks the user for a value) *)
  | Num of basic_number
  | Expr of arithm list

(* Text-mode displaying functions *)
type textmode =
  | Text of string
  | Locate of int * int * string

(* Graphic screen functions *)
type graphic =
  | ViewWindow of int * int * int * int * int * int
  | PlotOn of int * int
  | PlotOff of int * int
  | Fline of int * int * int * int
  | DrawStat of int list * int list
  | GraphicText of int * int * string
  | Graphic_Function of string

(* Type for Basic commands *)
type command =
  | End (* End of the program *)
  | Empty (* Empty command, to complete the basic_code array *)
  | Expr of basic_expr (* Arithmetic expression *)
  | String of string list (* Text, stored as list of lexemes (strings of 1 or 2 characters) *)
  | Assign of basic_expr * variable (* expr -> X *)
  | AssignStruct of data_struct * variable (* [1,2,3] -> List A *)
  | Disp (* Display the result of the line above *)
  | Goto of int (* Jump to line l on the array *)
  | Prog of string (* Prog(name): jump to program name, then come back *)
  | If of basic_expr * int (* If(expr,l): If expr Then continue (else jump to line l) *)
  | JumpIf of basic_expr * int (* JumpIf(expr,l): If expr Then jump to line l *)
  | TextMode of textmode (* String, Locate, Disp... *)
  | Graphic of graphic (* Graphic screen functions *)
  | Function of string (* Any other function, stored in a hashtable *)

(* Type for compiled Basic code *)
(* An object of type basic_code has the form (code, progindex),
  where code is the compiled code of all the programs (separated by End)
  and progindex is a list of the name and index (in the code) of each program *)
type basic_code = (command array * (string * int) list);;
