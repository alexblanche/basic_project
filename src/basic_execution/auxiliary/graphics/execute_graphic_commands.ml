(* Execution of the graphic commands *)

let apply_graphic (ren : Sdlrender.t) (p : parameters) (i : int) (g : graphic)
  (text_screen : bool ref) (verbose : bool) : unit =
  
  match g with
    | Fline (ex1, ey1, ex2, ey2, style) ->
      let zx1 = eval_num p ex1 in
      let zy1 = eval_num p ey1 in
      let zx2 = eval_num p ex2 in
      let zy2 = eval_num p ey2 in
      let (a1,b1) = rescale p zx1.re zy1.re in
      let (a2,b2) = rescale p zx2.re zy2.re in
      if a1 >= 1 && a1 <= 127 && a2 >= 1 && a2 <= 127
        && b1 >= 1 && b1 <= 63 && b2 >= 1 && b2 <= 63 then
        (fline ren p a1 b1 a2 b2 style;
        refresh_update ren p !text_screen;
        if slowdown_condition () then
          Unix.sleepf timer.fline;
        text_screen := false)

    | Graphic_Function ("RCLPICT", [e]) ->
      let z = eval_num p e in
      if is_int z && z.re >= 1. && z.re <= 20. then
        (draw_pict ren p (int_of_complex z - 1);
        refresh_update ren p !text_screen;
        if slowdown_condition () then
          Unix.sleepf timer.rlcpict;
        text_screen := false)
      else graphic_fail i "Wrong index for RclPict command"

    | Graphic_Function ("STOPICT", [e]) ->
      let z = eval_num p e in
      if is_int z && z.re >= 1. && z.re <= 20. then
        let m = Array.make_matrix 64 128 false in
        (for b = 0 to 63 do
          for a = 0 to 127 do
            m.(b).(a) <- bgscreen.(b).(a) || gscreen.(b).(a)
          done
        done;
        let p_index = int_of_complex z - 1 in
        p.pict.(p_index) <- (2048, m);
        (* If the updated Pict was the BGPict, update at the next drawing *)
        if p.bgpict = p_index then
          background_changed := true)
      else graphic_fail i  "Wrong index for StoPict command"

    | Graphic_Function ("BGPICT", [e]) ->
      let z = eval_num p e in
      if is_int z && z.re >= 1. && z.re <= 20. then
        (p.bgpict <- int_of_complex z - 1;
        (* /!\ Observation:
           On the calculator, display after BGPict does NOT feature the background pict.
         *)
        (**********
        let (bgpict_size, m) = p.pict.(p.bgpict) in
        if bgpict_size >= 1024 then
          for b = 0 to 63 do
            for a = 0 to 127 do
              bgscreen.(b).(a) <- m.(b).(a) || bgscreen.(b).(a)
            done
          done;
        background_changed := true
        **********)
        refresh_update ren p !text_screen
        )
      else graphic_fail i  "Wrong index for BGPict command"

    | Graphic_Function ("RCLCAPT", [e]) ->
      let z = eval_num p e in
      if is_int z && z.re >= 1. && z.re <= 20. then
        (clear_graph ren;
        draw_frame ren;
        draw_single_pict_no_writing ren p.capt.(int_of_complex z - 1);
        refresh ren;
        (* disp_graphic ren true; *)
        (* Bug with erase_black_square *)
        line_feed ();
        clear_line !writing_index;
        locate_no_refresh ["e"; "n"; "o"; "D"] 17 !writing_index;
        wait_release ren false;
        wait_enter ren false;

        if not !text_screen then
          gdraw ren
        else 
          background_changed := true)
      else graphic_fail i "Wrong index for RclCapt command"

    | Text (ey, ex, se) ->
      (let zy = eval_num p ey in
      let zx = eval_num p ex in
      if not
        ((is_int zx) && (is_int zy)
        && (zx.re >= 1.) && (zx.re <= 127.)
        && (zy.re >= 1.) && (zy.re <= 63.))
        then
          graphic_fail i "Wrong arguments for Text";
      if zy.re <= 58. then
        (let _ =
          (* Exception to the treatment of other drawing commands,
             because a Text is also used to erase some pixels *)
          if !background_changed || !text_screen then
            (clear_graph ren;
            draw_frame ren;
            draw_black_square ren;
            draw_window ren p;
            draw_single_pict_no_writing ren gscreen;
            background_changed := false);
          match eval_str p se with
            | Str_content s -> draw_text ren (rev_lexlist_to_rev_symblist s false verbose) (int_of_complex zx) (int_of_complex zy)
            | Num_expr (Complex z) -> draw_number ren z p.polar (int_of_complex zx) (int_of_complex zy)
            | _ -> graphic_fail i  "Wrong output type for string expression evaluation"
        in
        text_screen := false;
        refresh ren;
        if slowdown_condition () then
          Unix.sleepf timer.text))

    | Graphic_Function ("DRAWSTAT", _) ->
      (Array.iter
        (fun (don, st, li1, li2, mk) ->
          (* The indices of the lists are fixed at Sgph definition: no string value here *)
          if don then
            (let l1 = get_val_listexpr p (VarList (Arithm [Entity (Value (complex_of_int li1))])) in
            let l2 = get_val_listexpr p (VarList (Arithm [Entity (Value (complex_of_int li2))])) in
            let pair_l = ref [] in
            Array.iter2
              (fun x y ->
                let zi = eval_num p x in
                let zj = eval_num p y in
                let i,j = rescale p zi.re zj.re in
                pair_l := (i,j) :: !pair_l)
              l1 l2;
            trace_drawstat ren !pair_l st mk))
        p.sgph;
      refresh_update ren p !text_screen;
      if slowdown_condition () then
        Unix.sleepf timer.drawstat;
      text_screen := false)

    | PlotOn (ex, ey) ->
      let zx = eval_num p ex in
      let zy = eval_num p ey in
      let (a,b) = rescale p zx.re zy.re in
      ((if a >= 1 && a <= 127 && b >= 1 && b <= 63 then
        (ploton ren gscreen a (64-b);
        (* Placing the coordinates in X, Y, as in Casio Basic *)
        set_var p.var 23 (complex_of_float zx.re);
        set_var p.var 24 (complex_of_float zy.re)));
      refresh_update ren p !text_screen;
      if slowdown_condition () then
        Unix.sleepf timer.plot;
      text_screen := false)

    | PlotOff (ex, ey) ->
      let zx = eval_num p ex in
      let zy = eval_num p ey in
      let (a,b) = rescale p zx.re zy.re in
      ((if a >= 1 && a <= 127 && b >= 1 && b <= 63 then
        ((* plotoff ren gscreen false a (64-b);
        plotoff ren bgscreen false a (64-b); *)
        (* No need to Sdlrender.fill_rect, since gdraw is called *)
        gscreen.(64-b).(a) <- false;
        bgscreen.(64-b).(a) <- false;
        (* Placing the coordinates in X, Y, as in Casio Basic *)
        set_var p.var 23 (complex_of_float zx.re);
        set_var p.var 24 (complex_of_float zy.re)));
      gdraw ren;
      if slowdown_condition () then
        Unix.sleepf timer.plot;
      text_screen := false)

    | Graphic_Function ("PLOTCHG", [ex; ey]) ->
      let zx = eval_num p ex in
      let zy = eval_num p ey in
      let (a,b) = rescale p zx.re zy.re in
      ((if a >= 1 && a <= 127 && b >= 1 && b <= 63 then
        (set_var p.var 23 (complex_of_float zx.re);
        set_var p.var 24 (complex_of_float zy.re);
        if gscreen.(64-b).(a) || bgscreen.(64-b).(a) then
          (plotoff ren gscreen false a (64-b);
          plotoff ren bgscreen false a (64-b))
        else ploton ren gscreen a (64-b)));
      gdraw ren;
      if slowdown_condition () then
        Unix.sleepf timer.plot;
      text_screen := false)

    | Graphic_Function ("PXLON", [ey; ex]) ->
      let a = int_of_complex (eval_num p ex) in
      let b = int_of_complex (eval_num p ey) in
      ((if a >= 1 && a <= 127 && b >= 1 && b <= 63 then
        (let x = approx_descale_x p a in
        let y = approx_descale_y p (64-b) in
        set_var p.var 23 (complex_of_float x);
        set_var p.var 24 (complex_of_float y);
        ploton ren gscreen a b));
      refresh_update ren p !text_screen;
      if slowdown_condition () then
        Unix.sleepf timer.plot;
      text_screen := false)

    | Graphic_Function ("PXLOFF", [ey; ex]) ->
      let a = int_of_complex (eval_num p ex) in
      let b = int_of_complex (eval_num p ey) in
      ((if a >= 1 && a <= 127 && b >= 1 && b <= 63 then
        (let x = approx_descale_x p a in
        let y = approx_descale_y p (64-b) in
        set_var p.var 23 (complex_of_float x);
        set_var p.var 24 (complex_of_float y);
        plotoff ren gscreen  false a b;
        plotoff ren bgscreen false a b));
      gdraw ren;
      if slowdown_condition () then
        Unix.sleepf timer.plot;
      text_screen := false)

    | Graphic_Function ("PXLCHG", [ey; ex]) ->
      let a = int_of_complex (eval_num p ex) in
      let b = int_of_complex (eval_num p ey) in
      ((if a >= 1 && a <= 127 && b >= 1 && b <= 63 then
        if gscreen.(b).(a) || bgscreen.(b).(a) then
          (let x = approx_descale_x p a in
          let y = approx_descale_y p (64-b) in
          set_var p.var 23 (complex_of_float x);
          set_var p.var 24 (complex_of_float y);
          plotoff ren gscreen  false a b;
          plotoff ren bgscreen false a b)
        else ploton ren gscreen a b);
      gdraw ren;
      if slowdown_condition () then
        Unix.sleepf timer.plot;
      text_screen := false)

    | Graphic_Function ("CLS", _) ->
      (wipe gscreen;
      draw_window ren p;
      line_feed ();
      clear_line !writing_index;
      locate_no_refresh ["e"; "n"; "o"; "D"] 17 !writing_index;
      background_changed := true)

    | ViewWindow el ->
      let xl =
        List.map
          (fun e ->
            let z = eval_num p e in
            if z.im <> 0.
              then graphic_fail i "ViewWindow expects real arguments"
              else z.re)
          el
      in
      partial_vwin ren p xl

    | Drawstat_Setup (sgphi, drawon, style_opt, list1_opt, list2_opt, mark_opt) ->
      (* Current parameters: used as default when some parameters are omitted *)
      let (curr_drawon, curr_style, curr_list1, curr_list2, curr_mark) = p.sgph.(sgphi) in
      
      (* Auxiliary function that returns the index of a List *)
      let get_list_index e =
        match e with
          | Arithm [Entity n] -> int_of_complex (eval_num p e)
          | StringExpr (Str_content sl) ->
            (try
              1 + list_index_from_string p.listfile p.listzero sl
            with
              | Not_found -> graphic_fail i "List string index not found in Sgph DrawStat setup")
          | _ -> graphic_fail i "Wrong List argument in Sgph DrawStat setup"
        in

      (match style_opt, list1_opt, list2_opt, mark_opt with
        | None, None, None, None ->
          p.sgph.(sgphi) <- (drawon, curr_style, curr_list1, curr_list2, curr_mark)
        | Some style, None, None, None ->
          p.sgph.(sgphi) <- (drawon, style, curr_list1, curr_list2, curr_mark)
        | Some style, Some list1, None, None ->
          let l1 = get_list_index list1 in
          p.sgph.(sgphi) <- (drawon, style, l1, curr_list2, curr_mark)
        | Some style, Some list1, Some list2, None ->
          let l1 = get_list_index list1 in
          let l2 = get_list_index list2 in
          p.sgph.(sgphi) <- (drawon, style, l1, l2, curr_mark)
        | Some style, Some list1, Some list2, Some mark ->
          let l1 = get_list_index list1 in
          let l2 = get_list_index list2 in
          p.sgph.(sgphi) <- (drawon, style, l1, l2, mark)
        | _ -> graphic_fail i "Wrong arguments for Sgph DrawStat setup")

    | Graphic_Function ("AXESON", _) ->
      (background_changed := true;
      p.axeson <- true)
    | Graphic_Function ("AXESOFF", _) ->
      (background_changed := true;
      p.axeson <- false)
    
    | Graphic_Function ("BGNONE", _) ->
      p.bgpict <- -1

    | Graphic_Function ("VERTICAL" as s, [style_code; e])
    | Graphic_Function ("HORIZONTAL" as s, [style_code; e]) ->
      let z_style =
        match style_code with
          | Complex z -> z
          | _ -> graphic_fail i ("Wrong style argument for "^(String.capitalize_ascii (String.lowercase_ascii s)))
      in
      let style =
        match int_of_complex z_style with
          | 0 -> None
          | 1 -> Some StyleNormal
          | 2 -> Some StyleThick
          | 3 -> Some StyleBroken
          | _ -> Some StyleDot
      in
      let z = eval_num p e in
      ((if s = "VERTICAL" then
        (let a = rescale_x p z.re in
        if a >= 1 && a <= 127 then
          fline ren p a 1 a 63 style)
      else (* Horizontal *)
        (let b = rescale_y p z.re in
        if b >= 1 && b <= 63 then
          fline ren p 1 b 127 b style));
      refresh_update ren p !text_screen;
      if slowdown_condition () then
        Unix.sleepf timer.fline;
      text_screen := false)

    | Graphic_Function ("STOVWIN", [e]) ->
      let z = eval_num p e in
      if is_int z then
        let a = int_of_complex z in
        if a >= 1 && a <= 6 then
          (p.vwin.(a-1) <-
            (access_real_var p.var xmin_index,
            access_real_var p.var xmax_index,
            access_real_var p.var xscl_index,
            access_real_var p.var ymin_index,
            access_real_var p.var ymax_index,
            access_real_var p.var yscl_index))
        else graphic_fail i "StoVWin index should be between 1 and 6"
      else graphic_fail i "StoVWin expects an integer index"

    | Graphic_Function ("RCLVWIN", [e]) ->
      let z = eval_num p e in
      if is_int z then
        let a = int_of_complex z in
        if a >= 1 && a <= 6 then
          let (xmin, xmax, xscl, ymin, ymax, yscl) = p.vwin.(a-1) in
          viewWindow ren p xmin xmax xscl ymin ymax yscl
        else graphic_fail i "RclVWin index should be between 1 and 6"
      else graphic_fail i "RclVWin expects an integer index"

      | Graphic_Function ("CIRCLE", [style_code; ex; ey; er]) ->
        let z_style =
          match style_code with
            | Complex z -> z
            | _ -> graphic_fail i "Wrong style argument for Circle"
        in
        let style =
          match int_of_complex z_style with
            | 0 -> None
            | 1 -> Some StyleNormal
            | 2 -> Some StyleThick
            | 3 -> Some StyleBroken
            | _ -> Some StyleDot
        in
        let zx = eval_num p ex in
        let zy = eval_num p ey in
        let zr = eval_num p er in
        (text_screen := false;
        if zx.im <> 0. || zy.im <> 0. || zr.im <> 0. then
          graphic_fail i "Circle expects real arguments";
        circle ren p zx.re zy.re zr.re style)

    | Graphic_Function ("SLNORMAL", []) -> p.style <- StyleNormal
    | Graphic_Function ("SLTHICK", []) -> p.style <- StyleThick
    | Graphic_Function ("SLBROKEN", []) -> p.style <- StyleBroken
    | Graphic_Function ("SLDOT", []) -> p.style <- StyleDot

    | Graph (graph_var, graph_type, [e]) ->
      (match graph_var with
        | 'Y' -> graphy ren p text_screen ("GRAPHY"^graph_type) e false 0. 0.
        | 'X' -> graphx ren p text_screen ("GRAPHX"^graph_type) e
        | 'R' -> (* to do *) ()
        | _ -> graphic_fail i "Wrong variable for Graph display")

    (* Temporary (for Clonelab) *)
    | Graph ('S', _, [e; exmin; exmax]) ->
      let xmin = let z = eval_num p exmin in z.re in
      let xmax = let z = eval_num p exmax in z.re in
      graphy ren p text_screen "GRAPHYLEQ" e true xmin xmax

    | Graphic_Function ("CLRGRAPH", []) ->
      (* Equivalent to ViewWindow -6.3, 6.3, 1, -3.1, 3.1, 1 (empirically) *)
      (set_real_var p.var xmin_index (-6.3);
      set_real_var p.var xmax_index 6.3;
      set_real_var p.var xscl_index 1.;
      set_real_var p.var ymin_index (-3.1);
      set_real_var p.var ymax_index 3.1;
      set_real_var p.var yscl_index 1.;
      set_real_var p.var xdot_index 0.1;
      wipe gscreen;
      draw_window ren p;
      background_changed := true)

    (* Ignored commands *)
    | Graphic_Function ("FUNCOFF", [])
    | Graphic_Function ("GRIDOFF", [])
    | Graphic_Function ("LABELOFF", [])
    | Graphic_Function ("SWINDMAN", [])
    | Graphic_Function ("COORDOFF", []) -> ()

    (* Errors or functionalities not implemented yet *)
    | Graphic_Function (s, _) ->
      if verbose then
        print_endline ("Runtime warning: ignored command "^s)
    | Graph (_,_,_) -> graphic_fail i "Wrong graph command"
    (* | _ -> failwith "Runtime error: wrong parameters for a graphic command" *)
;;

