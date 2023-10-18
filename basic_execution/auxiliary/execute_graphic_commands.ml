(* Execution of the graphic commands *)

let apply_graphic (ren : Sdlrender.t) (p : parameters) (g : graphic) (text_screen : bool ref) : unit =
  match g with
    | Fline (ex1, ey1, ex2, ey2, style) ->
      (* To do: implement styles *)
      let zx1 = eval_num p ex1 in
      let zy1 = eval_num p ey1 in
      let zx2 = eval_num p ex2 in
      let zy2 = eval_num p ey2 in
      let (a1,b1) = rescale p zx1.re zy1.re in
      let (a2,b2) = rescale p zx2.re zy2.re in
      if a1 >= 1 && a1 <= 127 && a2 >= 1 && a2 <= 127
        && b1 >= 1 && b1 <= 63 && b2 >= 1 && b2 <= 63 then
        (let rect_t = Array.of_list (bresenham true gscreen a1 (64-b1) a2 (64-b2)) in
        Sdlrender.fill_rects ren rect_t;
        text_screen := false;
        refresh_update ren p)

    | Graphic_Function ("RCLPICT", [e]) ->
      let z = eval_num p e in
      if is_int z && z.re >= 1. && z.re <= 20. then
        (text_screen := false;
        draw_pict ren p (int_of_complex z - 1);
        refresh_update ren p)
      else failwith "Graphic error: wrong index for RclPict command"

    | Graphic_Function ("STOPICT", [e]) ->
      let z = eval_num p e in
      if is_int z && z.re >= 1. && z.re <= 20. then
        let m = Array.make_matrix 64 128 false in
        (for j = 0 to 63 do
          for i = 0 to 127 do
            m.(j).(i) <- gscreen.(j).(i)
          done
        done;
        p.pict.(int_of_complex z - 1) <- (2048, m))
      else failwith "Graphic error: wrong index for StoPict command"

    | Graphic_Function ("BGPICT", [e]) ->
      let z = eval_num p e in
      if is_int z && z.re >= 1. && z.re <= 20. then
        (p.bgpict <- int_of_complex z - 1;
        background_changed := true)
      else failwith "Graphic error: wrong index for BGPict command"

    | Text (ey, ex, se) ->
      let zy = eval_num p ey in
      let zx = eval_num p ex in
      if not
        ((is_int zx) && (is_int zy)
        && (zx.re >= 1.) && (zx.re <= 127.)
        && (zy.re >= 1.) && (zy.re <= 63.))
        then failwith "Graphic error: wrong arguments for Text";
      
      if zy.re <= 58. then
        (text_screen := false;
        let _ =
          (* Exception to the treatment of other drawing commands,
             because a Text is also used to erase some pixels *)
          clear_graph ren;
          draw_frame ren;
          draw_window ren p;
          background_changed := false;
          match eval_str p se with
            | Str_content s -> draw_text ren (rev_lexlist_to_rev_symblist s false) (int_of_complex zx) (int_of_complex zy)
            | Num_expr (Complex z) -> draw_number ren z p.polar (int_of_complex zx) (int_of_complex zy)
            | _ -> failwith "Graphic error: wrong output type for string expression evaluation"
        in
        refresh ren)

    | Graphic_Function ("DRAWSTAT", _) ->
      (Array.iter
        (fun (don, st, li1, li2, mk) ->
          if don then
            let l1 = get_val_listexpr p (VarList (Value (complex_of_int li1))) in
            let l2 = get_val_listexpr p (VarList (Value (complex_of_int li2))) in
            let pair_l = ref [] in
            Array.iter2
              (fun x y ->
                let zi = eval_num p x in
                let zj = eval_num p y in
                let i,j = rescale p zi.re zj.re in
                pair_l := (i,j) :: !pair_l)
              l1 l2;
            trace_drawstat ren !pair_l st mk)
        p.sgph;
      text_screen := false;
      refresh_update ren p)

    | PlotOn (ex, ey) ->
      (* let _ = print_endline "PLOT" in *)
      let zx = eval_num p ex in
      let zy = eval_num p ey in
      let (a,b) = rescale p zx.re zy.re in
      ((if a >= 1 && a <= 127 && b >= 1 && b <= 63 then
        ploton ren gscreen a (64-b));
      text_screen := false;
      refresh_update ren p)

    | PlotOff (ex, ey) ->
      let zx = eval_num p ex in
      let zy = eval_num p ey in
      let (a,b) = rescale p zx.re zy.re in
      ((if a >= 1 && a <= 127 && b >= 1 && b <= 63 then
        plotoff ren gscreen false a (64-b));
      text_screen := false;
      refresh_update ren p)

    | Graphic_Function ("CLS", _) ->
      (* (print_endline "CLS"; *)
      (wipe gscreen;
      background_changed := true)

    | ViewWindow (ex1, ex2, esx, ey1, ey2, esy) ->
      (* let _ = print_endline "VIEWWINDOW" in *)
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
          draw_window ren p;
          background_changed := true)
          (* No refresh: the will be refreshed when the first object will be drawn *)

    | Drawstat_Setup (sgphi, drawon, style_opt, list1_opt, list2_opt, mark_opt) ->
      let (curr_drawon, curr_style, curr_list1, curr_list2, curr_mark) = p.sgph.(sgphi) in
      (match style_opt, list1_opt, list2_opt, mark_opt with
        | None, None, None, None ->
          p.sgph.(sgphi) <- (drawon, curr_style, curr_list1, curr_list2, curr_mark)
        | Some style, None, None, None ->
          p.sgph.(sgphi) <- (drawon, style, curr_list1, curr_list2, curr_mark)
        | Some style, Some list1, None, None ->
          let z1 = eval_num p (Arithm [Entity list1]) in
          p.sgph.(sgphi) <- (drawon, style, int_of_complex z1, curr_list2, curr_mark)
        | Some style, Some list1, Some list2, None ->
          let z1 = eval_num p (Arithm [Entity list1]) in
          let z2 = eval_num p (Arithm [Entity list2]) in
          p.sgph.(sgphi) <- (drawon, style, int_of_complex z1, int_of_complex z2, curr_mark)
        | Some style, Some list1, Some list2, Some mark ->
          let z1 = eval_num p (Arithm [Entity list1]) in
          let z2 = eval_num p (Arithm [Entity list2]) in
          p.sgph.(sgphi) <- (drawon, style, int_of_complex z1, int_of_complex z2, mark)
        | _ -> failwith "Graphic error: wrong arguments for Sgph DrawStat setup")

    | Graphic_Function ("AXESON", _) ->
      (* (print_endline "AXESON"; *)
      (background_changed := true;
      p.axeson <- true)
    | Graphic_Function ("AXESOFF", _) ->
      (background_changed := true;
      p.axeson <- true)
    
    | Graphic_Function ("BGNONE", _) ->
      (background_changed := true;
      p.bgpict <- -1)

    | Graphic_Function ("HORIZONTAL", [Complex z]) ->
      let b = rescale_y p z.re in
      if b >= 1 && b <= 63 then
        (horizontal_line ren gscreen 1 127 b;
        text_screen := false;
        gdraw ren)

    | Graphic_Function ("VERTICAL", [Complex z]) ->
      let a = rescale_x p z.re in
      if a >= 1 && a <= 127 then
        (vertical_line ren gscreen a 1 63;
        text_screen := false;
        gdraw ren)

    | Graphic_Function ("SLNORMAL", []) -> p.style <- StyleNormal
    | Graphic_Function ("SLTHICK", []) -> p.style <- StyleThick
    | Graphic_Function ("SLBROKEN", []) -> p.style <- StyleBroken
    | Graphic_Function ("SLDOT", []) -> p.style <- StyleDot

    (* Errors or functionalities not implemented yet *)
    | Graphic_Function (s, _) -> (print_endline ("Runtime warning: ignored command "^s))
    (* | _ -> failwith "Runtime error: wrong parameters for a graphic command" *)
;;

