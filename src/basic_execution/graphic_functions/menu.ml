(* Menu command *)

(* Draws the frame of the menu *)
let draw_menu_frame (ren : Sdlrender.t) : unit =
  let rect_t =
    [|
      horizontal_rect 4 124 63;
      vertical_rect 4 1 63;
      vertical_rect 124 1 63;
      vertical_rect 125 2 63;
      horizontal_rect 4 124 1;
      horizontal_rect 5 123 12;
    |]
  in
  Sdlrender.fill_rects ren rect_t;;

(* Draws the title of the menu *)
let draw_menu_title (ren : Sdlrender.t) (title : string list) : unit =
  
  (* The text is shifted by 1 pixel horizontally and 3 pixels vertically for Locate 2,1 *)
  margin_v := !margin_v + 3 * !size;
  margin_h := !margin_h + !size;
  
  let acc = ref [] in
  fast_locate_aux false 0 0 acc title (min 20 (List.length title));
  Sdlrender.fill_rects ren (Array.of_list !acc);

  (* The margins are shifted back to their original position *)
  margin_v := !margin_v - 3 * !size;
  margin_h := !margin_h - !size;;

(* Draws the entries of the menu *)
(* The highest entry has index 'highest', and pindex is the current one. *)
let draw_menu_entries (ren : Sdlrender.t) (entries : (string list * int) array)
  (pindex : int) (highest : int) : unit =

  (* White rectangle over the entries *)
  set_color ren colors.background;
  let white_r =
    Sdlrect.make
      ~pos:(!margin_h + 5 * !size, !margin_v + 13 * !size)
      ~dims:(!size * 119, !size * 50)
  in
  Sdlrender.fill_rect ren white_r;
  set_color ren colors.pixels;

  (* The text is shifted by 2 pixels vertically *)
  margin_v := !margin_v - 2 * !size;

  let n = Array.length entries in
  let acc = ref [] in

  for i = 0 to min 5 (n-1 - highest) do
    let entry = fst (entries.(highest + i)) in
    if highest + i = pindex then
      ((* Black background *)
      fill_rect ren
        (!margin_h + 7 * !size) (!margin_v + (i+2) * 8 * !size)
        (114 * !size) (8 * !size);
      let white_acc = ref [] in
      (* "[index]:" *)
      margin_h := !margin_h + !size;
      fast_locate_aux false 1 (i+2) white_acc [":"; string_of_int (highest + i + 1)] 2;
      (* entry text *)
      fast_locate_aux false 3 (i+2) white_acc entry (min 20 (2 + List.length entry));
      margin_h := !margin_h - !size;
      set_color ren colors.background;
      Sdlrender.fill_rects ren (Array.of_list !white_acc);
      set_color ren colors.pixels)
    else
      (* "[index]:" *)
      (margin_h := !margin_h + !size;
      fast_locate_aux false 1 (i+2) acc [":"; string_of_int (highest + i + 1)] 2;
      (* entry text *)
      fast_locate_aux false 3 (i+2) acc entry (min 20 (2 + List.length entry));
      margin_h := !margin_h - !size)
  done;

  Sdlrender.fill_rects ren (Array.of_list !acc);
  
  (* The margin is shifted back to its original position *)
  margin_v := !margin_v + 2 * !size;;

(* Draws the initial menu *)
let draw_menu (ren : Sdlrender.t) (title : string list) (entries : ((string list) * int) array)
  (pindex : int) (highest : int) : unit =

  set_color ren colors.background;
  let white_r =
    Sdlrect.make
      ~pos:(!margin_h + 5 * !size, !margin_v + !size)
      ~dims:(!size * 119, !size * 61)
  in
  Sdlrender.fill_rect ren white_r;
  set_color ren colors.pixels;

  draw_menu_frame ren;
  draw_menu_title ren title;
  draw_menu_entries ren entries pindex highest;

  refresh ren;;

(* Executes the menu command *)
(* Takes a list of strings and label index as argument,
  the index is between 0 and 37: 0-9 = digits, 10-37 = letters,
  returns the chosen index *)
let menu_command (ren : Sdlrender.t) (p : parameters)
  (title : string list) (entries : ((string list) * int) array)
  (text_screen : bool) : int =
  
  wait_release ren text_screen;
  (if text_screen
    then erase_black_square_text ren
    else erase_black_square_graphic ren);
  draw_menu ren title entries 0 0;

  let n = Array.length entries in

  let rec loop (pindex : int) (highest : int) : int =
    (* Window closed *)
    if !exit_key_check then
      snd (entries.(0))
    else

    (* Resizing (messy) *)
    let _ =
      if !parameters_updated then
        (parameters_updated := false;
        clear_graph ren;
        draw_frame ren;
        (if text_screen then
          (* tdraw without draw_black_square and refresh *)
          (let acc = ref [] in
          for j = 0 to 6 do
            for i = 0 to 20 do
              if tscreen.(j).(i) <> "\000" then
                let t =
                  try
                    Hashtbl.find repr_text tscreen.(j).(i)
                  with
                    | Not_found -> Hashtbl.find repr_text "\064"
                in
                for y = 0 to 6 do
                  for x = 0 to 4 do
                    if t.(5*y+x) then
                      acc := (ploton_rect (1+6*i+x) (8*j+y))::!acc
                  done
                done
            done
          done;
          Sdlrender.fill_rects ren (Array.of_list !acc))
        else
          (* gdraw without draw_black_square and refresh *)
          (draw_single_pict_no_writing ren bgscreen;
          draw_single_pict_no_writing ren gscreen));
        draw_menu ren title entries pindex highest;
        refresh ren)
    in

    match !key_pressed with
      | Up ->
        (let new_pindex, new_highest =
          if pindex <> 0 then
            (pindex - 1, min (pindex - 1) highest)
          else
            (n-1, max (n-6) 0)
        in
        draw_menu_entries ren entries new_pindex new_highest;
        refresh ren;
        wait_release ren false;
        loop new_pindex new_highest)

      | Down ->
        (let new_pindex, new_highest =
          if pindex <> n-1 then
            (pindex + 1,
              if highest = pindex - 5
                then highest + 1
                else highest)
          else
            (0, 0)
        in
        draw_menu_entries ren entries new_pindex new_highest;
        refresh ren;
        wait_release ren false;
        loop new_pindex new_highest)

      | Return
      | KP_Enter -> pindex

      | _ -> loop pindex highest
  in
  
  let chosen_pindex = loop 0 0 in
  snd (entries.(chosen_pindex))
;;
