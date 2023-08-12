(* Abstract Basic program type *)
(* To be completed *)

(* Due to the presence of Lbl/Goto, a Casio Basic program cannot be represented as an
  abstract tree. *)
(* Each command is represented with an abstract command type.
  The program is compiled into an array of commands.
  - Patterns "Lbl A ... Goto A" are replaced with "Goto(l)",
  where l is the line where the Lbl A is (even if it is not
  actually present in the array). The lines of each Lbl are stored in a table. 
  - Conditionals (If) and loops (For, While, Do-LpWhile) are transformed with the Goto method. *)

(* Lists, Matrices, Strings *)
type data_struct =
  | List of int
  | Mat of char
  | Str of int

(* Type for Basic variables *)
type variable =
  | Var of char (* A ... Z, r, t = theta, @ = Ans *)
  | ListIndex of data_struct * int (* ListIndex(List(1), 3) = List 1[3] *)
  | MatIndex of data_struct * int (* MatIndex (Mat(3),i,j) -> Mat 3[i][j] *)
  | StrIndex of data_struct * int
  | Getkey (* its value depends on the key currently pressed *)

(***************************************************************************)
(** TO BE UNIFIED WITH THE TYPES OF arithmetic_parsing.ml **)
(* Type for Basic numbers *)
type basic_number =
  | Float of float
  | Var of variable

(* Type for Basic numerical and boolean expressions *)
type basic_expr =
  | QMark (* ? (asks the user for a value) *)
  | Num of arithm_lexeme list

(***************************************************************************)

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
(* Conditions are expressions: 0 = false, <>0 = true *)
type command =
  | Expr of basic_expr (* Arithmetic expression *)
  | String of string * bool (* Text *)
  | Assign of basic_expr * variable (* expr -> X *)
  | AssignStruct of data_struct * variable (* [1,2,3] -> List A *)
  | Disp (* Displays the result of the line above *)
  | Goto of int (* Jump to line l on the array *)
  | If of basic_expr * int (* If(expr,l): If expr Then continue (else jump to line l) *)
  | JumpIf of basic_expr * int (* JumpIf(expr,l): If expr Then jump to line l *)
  | TextMode of textmode (* String, Locate, Disp... *)
  | Graphic of graphic (* Graphic screen functions *)
  | Function of string (* Any other function, stored in a hashtable *)