(* General functions related to graphic display *)

(* Opens the graphic window and returns it along with its render *)
let open_graphic () : Sdlwindow.t * Sdlrender.t =
  let (win, ren) = config false (fun _ -> ()) in
  Sdlwindow.set_title ~window:win ~title:"Basic Emulator";
  clear_graph ren;
  (win, ren);;

(* Erases the pixels of the matrix m *)
let wipe (m : bool array array) : unit =
  for j = 0 to 63 do
    for i = 0 to 127 do
      m.(j).(i) <- false
    done;
  done;;

(* Returns only the rectangle, so that sdlrender.fill_rects can be used instead of fill_rect *)
let fast_ploton (i : int) (j : int) : Sdlrect.t =
  Sdlrect.make2 ~pos:(!margin_h + !size*i, !margin_v + !size*j) ~dims:(!size, !size);;