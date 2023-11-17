(* Line tracing, with thick, dot and broken styles *)

(* Truncates a rectangle to fit in the gscreen *)
(* When it is "too far" (test how much), the line is not drawn *)
let truncate (r : Sdlrect.t) : Sdlrect.t =
  let (i,j,w,h) = pixels_of_rectangle r in
	if i >= 1 && i <= 127 && w >= 1 && w <= 128-i
	&& j >= 1 && j <= 63 && h >= 1 && h <= 64-j
    then r
    else
      let ni = min (max i 1) 127 in
      let nj = min (max j 1) 63 in
      let nw = min (w-(abs (ni-i))) (128-ni) in
      let nh = min (h-nj+j) (64-nj) in
      Sdlrect.make
        ~pos:(!margin_h + !size * ni, !margin_v + !size * nj)
        ~dims:(!size * nw, !size * nh);;

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
                  let new_rects =
                    if ia >= 1 && ia <= 127 && ib >= 1 && ib <= 127
                      && ja >= 1 && ja <= 63 && jb >= 1 && jb <= 63
                    (* Both endpoints are in the screen *)
                      then bresenham true gscreen ia (64-ja) ib (64-jb)
                    (* One or both points do not belong to the screen *)
                      (* The distance between the points is not "too big"
                        (need experiments to obtain the exact condition) *)
                      else if (ib-ia)*(ib-ia) + (jb-ja)*(jb-ja) <= 500 then
                        ((* Truncate the rectangles that go out of the screen *)
                        let tr_br = List.rev_map truncate (bresenham false [||] ia (64-ja) ib (64-jb)) in
                        (* Write the pixels in gscreen (all within bounds) *)
                        List.iter (fun r -> write_in_matrix gscreen r) tr_br;
                        tr_br)
                      else
                        []
                  in
                  ((ib,jb), List.rev_append new_rects rect_l))
                (ij,[]) t
            in
            Sdlrender.fill_rects ren (Array.of_list rect_l))
      | Scatter -> ()
  in
  let _ =
    match mark with
      | DSMDot ->
        if style <> XYLine then
          List.iter
            (fun (i,j) ->
              if i >= 1 && i <= 127 && j >= 1 && j <= 63 then
                ploton ren gscreen i (64-j))
            l
      | DSMCross ->
        List.iter
          (fun (i,j) ->
            if i >= 1 && i <= 127 && j >= 1 && j <= 63 then
              for k = (-1) to 1 do
                let a = i + k in
                let b1 = j + k in
                let b2 = j - k in
                if a >= 1 && a <= 127 then
                  (if b1 >= 1 && b1 <= 63 then
                    ploton ren gscreen (i + k) (64 - b1);
                  if b2 >= 1 && b2 <= 63 then
                    ploton ren gscreen (i + k) (64 - b2))
              done)
          l
      | DSMSquare ->
        List.iter
          (fun (i,j) ->
            if i >= 1 && i <= 127 && j >= 1 && j <= 63 then
              for kx = (-1) to 1 do
                for ky = (-1) to 1 do
                  if kx <> 0 || ky <> 0 then
                    let a = i + kx in
                    let b = j + ky in
                    if a >= 1 && a <= 127 && b >= 1 && b <= 63 then
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

(* General line drawing function *)

(* Draws the F-Line between the points (a1, b1) and (a2, b2) with given style *)
let fline (ren : Sdlrender.t) (p : parameters) (a1 : int) (b1 : int) (a2 : int) (b2 : int)
  (style : style option) : unit =
  
  (* The pixels are written in gscreeen below *)
  let rect_l = bresenham false [||] a1 (64-b1) a2 (64-b2) in
  (* Computing the right rectangles to draw, depending on the style of line *)
  let rect_t =
    let st =
      match style with
        | None -> p.style
        | Some s -> s
    in
    match st with
      | StyleNormal -> Array.of_list rect_l
      | StyleThick -> Array.of_list (List.rev_map thicken rect_l)
      | StyleDot ->
        (* Condition for keep_last checked experimentally, see run_style tests/tests_run.ml *)
        Array.of_list (dot_line rect_l a1 b1 a2 b2 (a2 >= a1 && (b2 < b1 || b2+a1 <= a2+b1)))
      | StyleBroken ->
        (* Condition for index checked experimentally, see run_broken and run_broken2 tests/tests_run.ml *)
        let nb = max (abs (a2-a1)) (abs (b2-b1)) + 1 in
        let index =
          if a2 > a1 && b2+a1 <= a2+b1
            || b2 < b1 && not (a2 < a1 && b1+a2 < a1+b2) 
            then 1
            else nb mod 3
        in
        Array.of_list (List.rev_map thicken (broken_line (List.rev rect_l) a1 b1 a2 b2 index))
  in
  (* Writting the pixels in gscreen *)
  Array.iter (fun r -> write_in_matrix gscreen r) rect_t;
  (* Drawing the pixels *)
  Sdlrender.fill_rects ren rect_t;;