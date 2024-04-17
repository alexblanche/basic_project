(* Types for arithmetic expression *)

(* Integration of list/matrix arithmetic:
  Separate num_expr, list_expr and mat_expr at lexing time,
  each made up of a basic_expr containing some variables List _, Mat _, {...} and [[][]]

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
  | VarList of basic_expr
    (* List _ 
      Accepts Arithm [Entity (Value _)] (List 2),
      Arithm [Entity (Variable _)] (List B),
      or StringExpr (Str_content _) (List "ABC") *)
  | VarMat of int (* Mat _ *)

and

(* Type for Basic variables *)
variable =
  | Var of int (* index: 0..25 = A ... Z, 26 = r, 27 = theta, 28 = Ans *)
  | ListIndex of basic_expr * num_expr (* ListIndex(x, e) = List x[e], x = value, variable, string *)
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
  | Lunop of (string * bool)
    (* Left unary operator,
      the boolean is true if it is an arithmetic function
      (so the operator must be applied to each term of a list),
      false if it is an entity function (it should be applied to the whole list) *)
  | Function of string * (basic_expr list) (* Function and list of arguments *)
  | Comma (* , *)

and

(* String expressions *)
string_expr =
  | Num_expr of num_expr (* numerical expression *)
  | Str_content of string list (* explicit definition of a string *)
  | Str_access of int (* Str i *)
  | Str_Func of string * string_expr list (* Function applied to several string_expr objects *)
  | ListIndexZero of basic_expr (* List x[0], x = value, variable, string *)

and

(* Type for Basic numerical and boolean expressions *)
(* Conditions are expressions: 0 = false, <>0 = true *)
basic_expr =
  | QMark (* ? (asks the user for a value) *)
  | Arithm of arithm list (* Arithmetic expression *)
  | Complex of complex (* Single complex value *)
  | StringExpr of string_expr (* String expression *)
  
(* Specific types for numerical expressions, list expressions and matrix expressions *)
and num_expr = basic_expr
and list_expr = basic_expr
and mat_expr = basic_expr

(* Type for expressions *)
(* By default, Numerical is specified *)
type expression_type = Numerical | ListExpr | MatExpr


(** Accessors **)

(* Returns the value of List a *)
(* let get_list (tlist : float array array) (a : int) : float array =
  tlist.(if a = 28 then 26 else a);; *)

(* Returns the value of Mat a *)
let get_mat (tmat : float array array array) (a : int) : float array array =
  tmat.(if a = 28 then 26 else a);;

(* Returns the type of the entity, as an expression_type constructor *)
let entity_type (n : entity) : expression_type =
  match n with
    | Value _
    | Variable _ -> Numerical
    | ListContent _
    | VarList _ -> ListExpr
    | _ -> MatExpr
;;