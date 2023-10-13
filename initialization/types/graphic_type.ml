(* Types for graphic commands *)

type style = Normal | Thick | Dot | Broken

(* Graphic screen commands *)
type graphic =
  | ViewWindow of num_expr * num_expr * num_expr * num_expr * num_expr * num_expr
  | PlotOn of num_expr * num_expr
  | PlotOff of num_expr * num_expr
  | Fline of num_expr * num_expr * num_expr * num_expr * (style option)
  | Text of num_expr * num_expr * string_expr
  | Graphic_Function of string * (num_expr list) (* Other graphic functions *)