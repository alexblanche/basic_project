(* Types for arithmetic expression *)

(* Integration of list/matrix arithmetic:
  Separate num_expr, list_expr and mat_expr at lexing time,
  each made up of a basic_expr containing some variables List _, Mat _, {...} and [[][]]
  
  The output type of the lexing function is the following:
  type expression_type = Numerical | ListExpr | MatExpr
  type expression = basic_expr * expression_type

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

(* Type for expressions *)
(* By default, Numerical is specified *)
type expression_type = Numerical | ListExpr | MatExpr


(** Accessors **)

(* Returns the value of List a *)
let get_list (tlist : float array array) (a : int) : float array =
  tlist.(a);;

(* Returns the value of Mat a *)
let get_mat (tmat : float array array array) (a : int) : float array array =
  tmat.(a);;

(* is_number, is_list, is_mat:
  return true if the entity is a complex, a list or a matrix respectively *)
let is_number (n : entity) : bool =
  match n with
    | Value _
    | Variable _ -> true
    | _ -> false;;

let is_list (n : entity) : bool =
  match n with
    | ListContent _
    | VarList _ -> true
    | _ -> false;;

let is_mat (n : entity) : bool =
  match n with
    | MatContent _
    | VarMat _ -> true
    | _ -> false;;

(* Returns true if the two given entities have compatible types,
  i.e. two complexes, one complex and a list or a matrix,
  two lists of the same size, two matrices of the same size *)
  let have_compatible_types (n1 : entity) (n2 : entity) : bool =
    is_number n1
    || is_number n2
    || is_list n1 && is_list n2
    || is_mat n1 && is_mat n2;;