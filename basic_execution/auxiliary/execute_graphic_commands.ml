(* Execution of the graphic commands *)

let apply_graphic (ren : Sdlrender.t) (p : parameters) (g : graphic) : unit =
  match g with
    | Fline (ex1, ey1, ex2, ey2) ->
      let zx1 = eval_num p ex1 in
      let zy1 = eval_num p ey1 in
      let zx2 = eval_num p ex2 in
      let zy2 = eval_num p ey2 in
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
      p.pict.(int_of_complex z - 1) <- (2048,m))
    | Text (ey, ex, se) ->
      let zy = eval_num p ey in
      let zx = eval_num p ex in
      if not
        ((is_int zx) && (is_int zy)
        && (zx.re >= 1.) && (zx.re <= 127.)
        && (zy.re >= 1.) && (zy.re <= 63.))
        then failwith "Graphic error: wrong arguments for Text";
      (match eval_str p se with
        | Str_content s -> draw_text ren s (int_of_complex zx) (int_of_complex zy)
        | Num_expr (Complex z) -> draw_number ren z p.polar (int_of_complex zx) (int_of_complex zy)
        | _ -> failwith "Graphic error: wrong output type for string expression evaluation")

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
      (plotoff ren gscreen false a b;
      gdraw ren)
    | Graphic_Function ("CLS", _) ->
      wipe gscreen
    | ViewWindow (ex1, ex2, esx, ey1, ey2, esy) ->
      let xminval = eval_num p ex1 in
      let xmaxval = eval_num p ex2 in
      let xstepval = eval_num p esx in
      let yminval = eval_num p ey1 in
      let ymaxval = eval_num p ey2 in
      let ystepval = eval_num p esy in
      if xminval.im <> 0. || xmaxval.im <> 0. || xstepval.im <> 0.
        || yminval.im <> 0. || ymaxval.im <> 0. || ystepval.im <> 0.
        then failwith "Graphic error: ViewWindow expects real arguments"
        else
          (p.xmin <- xminval.re;
          p.xmax <- xmaxval.re;
          p.xstep <- xstepval.re;
          p.ymin <- yminval.re;
          p.ymax <- ymaxval.re;
          p.ystep <- ystepval.re;
          draw_window ren p)
    | Graphic_Function ("AXESON", _) -> p.axeson <- true
    | Graphic_Function ("AXESOFF", _) -> p.axeson <- false
    | Graphic_Function ("BGPICT", [Complex z]) ->
      p.bgpict <- int_of_complex z - 1
    | Graphic_Function ("BGNONE", _) -> p.bgpict <- -1
    | Graphic_Function ("HORIZONTAL", [Complex z]) ->
      let (_, b) = rescale p 0. z.re in
      (bresenham ren gscreen 1 b 127 b;
      gdraw ren)
    | Graphic_Function ("VERTICAL", [Complex z]) ->
      let (a, _) = rescale p z.re 0. in
      (bresenham ren gscreen a 1 a 63;
      gdraw ren)
    | _ -> failwith "Runtime error: wrong parameters for a graphic command"
;;

