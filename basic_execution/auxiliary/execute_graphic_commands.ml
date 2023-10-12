(* Execution of the graphic commands *)

let apply_graphic (g : graphic) : unit =
  match g with
    | PlotOn (ex, ey) ->
      let zx = eval_num p ex in
      let zy = eval_num p ey in
      let (a,b) = rescale p zx.re zy.re in
      ploton ren a b
    (* | ... *)
    | Function "CLS" ->
      wipe gscreen
    (* | ViewWindow ex1, ex2, esx, ey1, ey2, esy -> ... *)
;;

