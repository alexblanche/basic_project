(* Auxiliary functions to execute_graphic_commands.ml *)

let aux_drawstat (ren : Sdlrender.t) (l : (float * float) list) st mk =
  let _ =
    match style with
      | XYLine ->
        (match l with
          | [] -> ()
          | p::t -> 
            List.fold_left (fun (a,b) (x,y) -> bresenham ren gscreen a b x y; (x,y)) p t)
      | Scatter -> ()
  in
  let _ =
    match mk with
      | DSMDot ->
        if style <> XYLine then
          List.iter (fun (x,y) -> ploton ren gscreen x y) l
      | DSMCross ->
        List.iter
          (fun (x,y) ->
            for k=-1 to 1 do
              ploton ren gscreen (x + k) (y + k);
              ploton ren gscreen (x + k) (y - k)
            done)
          l
      | DSMSquare ->
        List.iter
          (fun (x,y) ->
            for ky = -1 to 1 do
              for kx = -1 to 1 do
                ploton ren gscreen (x + kx) (y + ky)
              done
            done)
          l
  in
  ();;