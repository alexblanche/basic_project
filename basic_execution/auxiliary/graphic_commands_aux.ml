(* Auxiliary functions to execute_graphic_commands.ml *)

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

(* To do: thick convert a horizontal line to thick horizontal line, same vertical,
   then general convert function plot -> thick plot, h/v line -> thick h/v line,
   then List.rev_map it *)

  
(* Erases one out of two elements of the list l, starting by the first
   if the length of l is odd, and the second one if the length is even
  Returns the list in reverse order *)
(* The obvious polymorphism is removed to help the OCaml compiler generate a faster machine code *)
let dot_erase_half (l : Sdlrect.t list) : Sdlrect.t list =
  let (_, a) =
    List.fold_left
      (fun (erase, acc) x ->
        if erase
          then (false, acc)
          else (true, x::acc))
      ((List.length l) mod 2 = 1, []) l
  in
  a;;