(* Auxiliary functions to execute_graphic_commands.ml *)

let trace_drawstat (ren : Sdlrender.t) (l : (int * int) list) (style : drawstat_style) (mark : drawstat_mark) : unit =
  let _ =
    match style with
      | XYLine ->
        (match l with
          | [] -> ()
          | ij::t -> 
            let _ =
              List.fold_left
                (fun (ia,ja) (ib,jb) ->
                  bresenham ren gscreen ia (64-ja) ib (64-jb); (ib,jb)) ij t
            in ())
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