(* Types for graphic commands *)

type style = StyleNormal | StyleThick | StyleDot | StyleBroken

type drawstat_mark = DSMSquare | DSMCross | DSMDot

type drawstat_style = Scatter | XYLine

(* Graphic screen commands *)
type graphic =
  | ViewWindow of num_expr * num_expr * num_expr * num_expr * num_expr * num_expr
  | PlotOn of num_expr * num_expr
  | PlotOff of num_expr * num_expr
  | Fline of num_expr * num_expr * num_expr * num_expr * (style option)
  | Text of num_expr * num_expr * string_expr
  | Drawstat_Setup of int * bool * drawstat_style * entity * entity * drawstat_mark
    (* Drawstat_Setup (i, drawon, ds_style, z1, z2, ds_mark)
      Sgph(i) drawon, ds_style, List z1, List z2, 1 (?), ds_mark *)
  | Graphic_Function of string * (num_expr list) (* Other graphic functions *)