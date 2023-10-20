(* Auxiliary functions to execute_graphic_commands.ml *)

(* Auxiliary function that traces the rectangles of the DrawStat display *)
let trace_drawstat (ren : Sdlrender.t) (l : (int * int) list) (style : drawstat_style) (mark : drawstat_mark) : unit =
  let _ =
    match style with
      | XYLine ->
        (match l with
          | [] -> ()
          | ij::t -> 
            let (_, rect_l) =
              List.fold_left
                (fun ((ia,ja), rect_l) (ib,jb) ->
                  ((ib,jb),
                  List.rev_append (bresenham true gscreen ia (64-ja) ib (64-jb)) rect_l)) 
                (ij,[]) t
            in
            Sdlrender.fill_rects ren (Array.of_list rect_l))
      | Scatter -> ()
  in
  let _ =
    match mark with
      | DSMDot ->
        if style <> XYLine then
          List.iter (fun (i,j) -> ploton ren gscreen i (64-j)) l
      | DSMCross ->
        List.iter
          (fun (i,j) ->
            for k = (-1) to 1 do
              ploton ren gscreen (i + k) (64 - j - k);
              ploton ren gscreen (i + k) (64 - j + k)
            done)
          l
      | DSMSquare ->
        List.iter
          (fun (i,j) ->
            for kx = (-1) to 1 do
              for ky = (-1) to 1 do
                if kx <> 0 || ky <> 0 then
                  ploton ren gscreen (i + kx) (64 - j - ky)
              done
            done)
          l
  in
  ();;

(** Styles implementation **)

(* Thick style *)

(* Returns a rectangle of 2 pixels wide and tall, with lower right corner
   being the pixel of coordinates (i,j) (used for Thick style) *)
let thick_style_square (i : int) (j : int) : Sdlrect.t =
  let ni = max (i-1) 1 in
  let nj = max (j-1) 1 in
  Sdlrect.make
    ~pos:(!margin_h + !size * ni, !margin_v + !size * nj)
    ~dims:((i-ni+1) * !size, (j-nj+1) * !size);;

(* Converts the given rectangle to a thickened version *)
let thicken_rect (i : int) (j : int) (w : int) (h : int): Sdlrect.t =
  let ni = max (i-1) 1 in
  let nj = max (j-1) 1 in
  Sdlrect.make
    ~pos:(!margin_h + !size * ni, !margin_v + !size * nj)
    ~dims:((min (w + i - ni) (128-ni)) * !size, (min (j - nj + h) (64-nj)) * !size);;

(* Conversion function that returns the thickened version of the rectangle *)
let thicken (rect : Sdlrect.t) : Sdlrect.t =
  let (i,j,w,h) = pixels_of_rectangle rect in
  thicken_rect i j w h;;



(* Dot style *)

(* Converts the horizontal (if w > 1) or vertical (if h > 1) line into a list of rectangles
   and keeps one out of 2 of them *)
(* If keep_first is true, the first pixel ((i,j)) is kept *)
let dot_hv_line (i : int) (j : int) (w : int) (h : int) (keep_last : bool) : Sdlrect.t list =
  let rects = ref [] in
  (if w > 1 then
    (* Horizontal line *)
    let x = ref (if keep_last then w-1 else w-2) in
    while !x >= 0 do
      rects := (ploton_rect (i + !x) j) :: !rects;
      x := !x - 2
    done
  else if h > 1 then
    (* Vertical line *)
    let y = ref (if keep_last then h-1 else h-2) in
    while !y >= 0 do
      rects := (ploton_rect i (j + !y)) :: !rects;
      y := !y - 2
    done
  else
    (* Single plot *)
    rects := (if keep_last then [ploton_rect i j] else []));
  !rects;;

(* Converts the bresenham-generated line (i1,j1)-(i2,j2) into a dotted line
   (both in rectangle list form) *)
let dot_line (l : Sdlrect.t list) (i1 : int) (j1 : int) (i2 : int) (j2 : int) (init_keep_last : bool): Sdlrect.t list =
  let nb = max (abs (i2-i1)) (abs (j2-j1)) + 1 in
  let (_, rect_l) =
    List.fold_left
      (fun (prev_keep_last, acc) r ->
        let (i,j,w,h) = pixels_of_rectangle r in
        let keep_last = (if w > 1 then w mod 2 = 0 else h mod 2 = 0) = prev_keep_last in
        (keep_last, List.rev_append (dot_hv_line i j w h keep_last) acc))
      (init_keep_last && nb mod 2 = 0, []) l
  in rect_l;;


(* Broken style *)
(* Not perfect, works on ~3/4 of the cases (see run_broken in tests/tests_run.ml) *)

let broken_hv_line (i : int) (j : int) (w : int) (h : int) (index : int) : Sdlrect.t list * int =
  let rects = ref [] in
  (* print_string "index : ";
  print_int index;
  print_newline (); *)
  if w > 1 then
    (* Horizontal line *)
    (let x = ref (i + w - index) in
    while !x >= i do
      (* print_string "ploton i + !x : ";
      print_int (i + !x);
      print_newline (); *)
      rects := (ploton_rect !x j) :: !rects;
      x := !x - 3
    done;
    (!rects, i - !x))
  else if h > 1 then
    (* Vertical line *)
    (let y = ref (j + h - index) in
    while !y >= j do
      (* print_string "ploton j + !y : ";
      print_int (j + !y);
      print_newline (); *)
      rects := (ploton_rect i !y) :: !rects;
      y := !y - 3
    done;
    (!rects, j - !y))
  else
    (* Single plot *)
    if index = 1 then 
      ([ploton_rect i j], 3)
    else
      ([], index-1);;

(* Converts the bresenham-generated line (i1,j1)-(i2,j2) into a dotted line
   (both in rectangle list form) *)
let broken_line (l : Sdlrect.t list) (i1 : int) (j1 : int) (i2 : int) (j2 : int)
  (init_index : int) : Sdlrect.t list =

  (* let nb = max (abs (i2-i1)) (abs (j2-j1)) + 1 in *)
  let (_, rect_l) =
    List.fold_left
      (fun (prev_index, acc) r ->
        let (i,j,w,h) = pixels_of_rectangle r in
        let (rectl, index) = broken_hv_line i j w h (if prev_index = 0 then 3 else prev_index) in
        (index, List.rev_append rectl acc))
      (init_index, []) l
  in rect_l;;


(****************************************************************************)
(** Graphs **)

(** Graph Y (=, >=, <=, >, <) **)

let graphy (ren : Sdlrender.t) (p : parameters) (text_screen : bool ref)
  (command : string) (e : num_expr) : unit =

  refresh_update ren p !text_screen;
  text_screen := false;
  let t =
    match command with
    (* t mod 2 = 0 <=> EQ type
       t <= 2 (and <> 0) <=> "greater than" type
       t >= 3 <=> "less than" type *)
      | "GRAPHYEQ" -> 0
      | "GRAPHYG" -> 1
      | "GRAPHYGEQ" -> 2
      | "GRAPHYL" -> 3
      | "GRAPHYLEQ" -> 4
      | _ -> -1
  in
  let step = (p.xmax -. p.xmin) /. 126. in
  (* xmin -> X *)
  p.var.(23+29) <- 0.;
  p.var.(23) <- p.xmin;
  let z = eval_num p e in
  let last_j = ref (rescale_y p z.re) in

  for i = 1 to 127 do
    let z = eval_num p e in
    let j = rescale_y p z.re in
    (if j >= 1 && j <= 63 then
      (* normal line if =, >= or <=, dot line if > or < *)
      let (jmin, jmax) =
        if !last_j < j
          then (max (!last_j + 1) 1, j)
        else if !last_j > j
          then (j, min (!last_j - 1) 63)
        else (j, j)
      in
      (vertical_line ren gscreen i (64-jmax) (64-jmin);
      (* Refreshing at each step in Casio Basic (if "=" mode) *)
      if t mod 2 = 0 then
        refresh ren)
    else if !last_j >= 1 && !last_j <= 63 then
      let (jmin, jmax) =
        if j <= 0 then (1, max (!last_j - 1) 1) else (min (!last_j + 1) 63, 63)
      in
      (vertical_line ren gscreen i (64-jmax) (64-jmin);
      (* Refreshing at each step in Casio Basic (if "=" mode) *)
      if t mod 2 = 0 then
        refresh ren)
    );
    last_j := j;
    if t<>0 then
      (if t<=2 (* > or >= *) then
        (let acc = ref [] in
        let parity = (i + 1) mod 2 in
        for k = max (j+1) 1 to 63 do
          if k mod 2 = parity then
            acc := (ploton_rect i (64-k)) :: !acc
        done;
        Sdlrender.fill_rects ren (Array.of_list !acc);
        refresh ren
        )
      else (* < or <= *)
        (let acc = ref [] in
        let parity = (i + 1) mod 2 in
        for k = min (j-1) 63 downto 1 do
          if k mod 2 = parity then
            acc := (ploton_rect i (64-k)) :: !acc
        done;
        Sdlrender.fill_rects ren (Array.of_list !acc);
        refresh ren
        )
      );
    p.var.(23) <- p.var.(23) +. step
  done;;