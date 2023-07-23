(* Abstract Basic tree type *)
(* To be completed *)

(* Major difficulty: how do I handle the Lbl/Goto? *)

(* Lists, Matrices, Strings *)
type data_struct =
  | List of int
  | Mat of char
  | Str of int

(* Type for Basic variables *)
type variable =
  | Var of char
  | ListIndex of data_struct * int (* ListIndex(List(1), 3) = List 1[3] *)
  | MatIndex of data_struct * int (* MatIndex (Mat(3),i,j) -> Mat 3[i][j] *)
  | StrIndex of data_struct * int

(* Type for Basic numbers *)
type basic_number =
  | Nint of int
  | Nfl of float
  | Var of variable

(* Type for Basic numerical and boolean expressions *)
type basic_expr =
  | QMark (* ? (asks the user for a value) *)
  | Getkey (* its value depends on the key currently pressed *)
  | Num of basic_number
  | Leq of basic_expr * basic_expr
  | Le of basic_expr * basic_expr
  | Geq of basic_expr * basic_expr
  | Ge of basic_expr * basic_expr
  | Equal of basic_expr * basic_expr

(* Text-mode displaying functions *)
type textmode =
  | Text of string
  | DispExpr of basic_expr (* (black triangle sign) displays the result of the expression *)
  | DispText of string (* same with a text *)
  | Locate of int * int * string

(* Graphic screen functions *)
type graphic =
  | ViewWindow of int * int * int * int * int * int
  | Graphic_seq of graphic list
  | PlotOn of int * int
  | PlotOff of int * int
  | Fline of int * int * int * int
  | DrawStat of int list * int list
  | GraphicText of int * int * string

(* Type for Basic code *)
(* Conditions are expressions: 0 = false, <>0 = true *)
type basic_code =
  | Seq of basic_code list (* Sequence of blocks of code *)
  | String of string * bool (* Text, boolean = true if the text is displayed with pause (black triangle) *)
  | Assign of basic_expr * variable (* expr -> X *)
  | AssignStruct of data_struct * variable (* [1,2,3] -> List A *)
  | If of basic_expr * basic_code (* If expr Then code IfEnd *)
  | Ifelse of basic_expr * basic_code * basic_code (* If expr Then code Else code IfEnd *)
  | While of basic_expr * basic_code (* While expr Do code WhileEnd *)
  | Do_LpWhile of basic_code * basic_expr (* Do code  *)
  | For of int * int * int * basic_code (* For i = X To Y Step Z code Next *)
  | TextMode of textmode (* String, Locate, Disp... *)
  | Graphic of graphic (* Graphic screen functions *)
  | Function of string (* Any other function, stored in a hashtable *)
