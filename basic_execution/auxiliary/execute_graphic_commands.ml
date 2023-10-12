(* Execution of the graphic commands *)

let apply_graphic (ren : Sdlrender.t) (p : parameters) (g : graphic) : unit =
  match g with
    | Fline (ex1, ey1, ex2, ey2) ->
      let zx1 = eval_num p ex in
      let zy1 = eval_num p ey in
      let zx2 = eval_num p ex in
      let zy2 = eval_num p ey in
      let (a1,b1) = rescale p zx1.re zy1.re in
      let (a2,b2) = rescale p zx2.re zy2.re in
      (bresenham ren gscreen a1 b1 a2 b2;
      gdraw ren)
    | Graphic_Function ("RCLPICT", [Complex z]) ->
      draw_pict ren p (int_of_complex z - 1)
    | Graphic_Function ("STOPICT", [Complex z]) ->
      let m = Array.make_matrix 64 128 false in
      (for j = 0 to 63 do
        for i = 0 to 127 do
          m.(j).(i) <- gscreen.(j).(i)
        done
      done;
      p.pict.(int_of_complex z - 1) <- m)
    | PlotOn (ex, ey) ->
      let zx = eval_num p ex in
      let zy = eval_num p ey in
      let (a,b) = rescale p zx.re zy.re in
      (ploton ren gscreen a b;
      gdraw ren)
    | PlotOff (ex, ey) ->
      let zx = eval_num p ex in
      let zy = eval_num p ey in
      let (a,b) = rescale p zx.re zy.re in
      (plotoff ren gscreen a b;
      gdraw ren)
    | Graphic_Function ("CLS", _) ->
      wipe gscreen
    | ViewWindow ex1, ex2, esx, ey1, ey2, esy ->
      (p.xmin <- eval_num p ex1;
      p.xmax <- eval_num p ex2;
      p.xstep <- eval_num p esx;
      p.ymin <- eval_num p ey1;
      p.ymax <- eval_num p ey2;
      p.ystep <- eval_num p esy;
      draw_window ren)
    | Graphic_Function ("AXESON", _) -> p.axeson <- true
    | Graphic_Function ("AXESOFF", _) -> p.axeson <- false
    | Graphic_Function ("BGPICT", [Complex z]) ->
      p.bgpict <- int_of_complex z - 1
    | Graphic_Function ("BGNONE", _) -> p.bgpict <- -1
;;

