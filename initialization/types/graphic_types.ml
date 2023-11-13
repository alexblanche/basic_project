(* Types for graphic commands *)

type style = StyleNormal | StyleThick | StyleDot | StyleBroken

type drawstat_mark = DSMSquare | DSMCross | DSMDot

type drawstat_style = Scatter | XYLine

(* Graphic screen commands *)
type graphic =
  | ViewWindow of num_expr list (* ViewWindow can take between 1 and 6 parameters *)
  | PlotOn of num_expr * num_expr
  | PlotOff of num_expr * num_expr
  | Fline of num_expr * num_expr * num_expr * num_expr * (style option)
  | Text of num_expr * num_expr * string_expr
  | Drawstat_Setup of int * bool * (drawstat_style option) * (entity option) * (entity option) * (drawstat_mark option)
    (* Drawstat_Setup (i, drawon, ds_style, z1, z2, ds_mark)
      Sgph(i) drawon, ds_style, List z1, List z2, 1 (?), ds_mark
      The last four parameters are optional, but the ones present must be in order, with none missing in-between *)
  | Graph of char * string * (num_expr list) (* Graph ('Y'|'X'|'R'|'S' , "EQ"|"G"|"L"|"GEQ"|"LEQ"|"" , [e;...]) *)
  | Graphic_Function of string * (num_expr list) (* Other graphic functions *)