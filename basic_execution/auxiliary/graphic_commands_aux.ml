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


  
(* Erases k-1 out of k elements of the list l, starting by the first
   if the length of l is odd, and the second one if the length is even
  Returns the list in reverse order *)
(* The obvious polymorphism is removed to help the OCaml compiler generate a faster machine code *)
let dot_erase (l : Sdlrect.t list) (k : int) : Sdlrect.t list =
  let (_, a) =
    List.fold_left
      (fun (cpt, acc) x ->
        if cpt<>0
          then (cpt-1, acc)
          else (k-1, x::acc))
      ((List.length l) mod k, []) l
  in
  a;;

(* Conversion to dotted line *)

(* Converts the horizontal (if w > 1) or vertical (if h > 1) line into a list of rectangles *)
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
(* Bresenham returns the list of rectangles in this order:
   [(i2,j2); (one rectangle not containing (i2,j2)); the other rectangles; (the rectangle containing i1,j1)]
   EXCEPT for the 1st and 8th octants, who are weirdly reversed in Casio Basic *)
let dot_line (l : Sdlrect.t list) (i1 : int) (j1 : int) (i2 : int) (j2 : int) : Sdlrect.t list =
  let (_, rect_l) =
    List.fold_left
      (fun (keep_first, acc) r ->
        let (i,j,w,h) = pixels_of_rectangle r in
        (* Trickery *)
        ((if w > 1 then w mod 2 = 0 else h mod 2 = 0) = keep_first,
        List.rev_append (dot_hv_line i j w h keep_first) acc))
      (true, []) l
  in rect_l;;