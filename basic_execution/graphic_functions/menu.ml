(* Menu command *)

(* Draws the frame of the menu *)
let draw_menu_frame (ren : Sdlrender.t) : unit =
  let rect_t =
    [| horizontal_rect 4 124 63;
      vertical_rect 4 1 63;
      vertical_rect 124 1 63;
      vertical_rect 125 1 62;
      horizontal_rect 4 125 1;
      horizontal_rect 5 123 52;
    |]
  in
  Sdlrender.fill_rects ren rect_t;;

(* Draws the title of the menu *)
let draw_menu_title (ren : Sdlrender.t) (title : string list) : unit =
  
  (* The text is shifted by 1 pixel horizontally and 3 pixels vertically for Locate 2,1 *)
  margin_v := !margin_v + 3 * !size;
  margin_h := !margin_h + !size;
  
  let acc = ref [] in
  fast_locate_aux 1 0 acc title 20;
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
      ~pos:(!margin_h + 5 * !size, !margin_v + 12 * !size)
      ~dims:(!size * 119, !size * 50)
  in
  Sdlrender.fill_rect ren white_r;
  set_color ren colors.pixels;

  (* The text is shifted by 1 pixel horizontally and 3 pixels vertically for Locate 2,1 *)
  margin_v := !margin_v + 3 * !size;
  margin_h := !margin_h + !size;

  let acc = ref [] in
  for i = 0 to 5 do
    let entry = fst (entries.(highest + i)) in
    if highest + i = pindex then
      (fill_rect ren !margin_h (!margin_v + i*8* !size) (128 * !size) (8* !size);
      switch_color_mode ();
      let white_acc = ref [] in
      (* Missing: "1:" "2:" *)
      fast_locate_aux 1 (i+1) white_acc entry 20;
      Sdlrender.fill_rects ren (Array.of_list !white_acc);
      switch_color_mode ())
    else
      (* Missing: "1:" "2:" *)
      fast_locate_aux 1 (i+1) acc entry 20
  done;

  Sdlrender.fill_rects ren (Array.of_list !acc);
  
  (* The margins are shifted back to their original position *)
  margin_v := !margin_v - 3 * !size;
  margin_h := !margin_h - !size;;

(* Draws the initial menu *)
let draw_menu (ren : Sdlrender.t) (title : string list) (entries : ((string list) * int) array) : unit =
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
  draw_menu_entries ren entries 0 0;

  refresh ren;;

(* Executes the menu command *)
(* Takes a list of strings and label index as argument,
  the index is between 0 and 37: 0-9 = digits, 10-37 = letters,
  returns the chosen index *)
let menu_command (ren : Sdlrender.t) (p : parameters)
  (title : string list) (entries : ((string list) * int) array) : int =
  
  draw_menu ren title entries;

  let n = Array.length entries in

  let rec loop (pindex : int) (highest : int) : int =
    match !key_pressed with
      | Up ->
        if pindex <> 0 then
          (let new_pindex = pindex - 1 in
          let new_highest = min new_pindex highest in
          draw_menu_entries ren entries new_pindex new_highest;
          refresh ren;
          loop new_pindex new_highest)
        else
          loop pindex highest

      | Down ->
        if pindex <> n-1 then
          (let new_pindex = pindex + 1 in
          let new_highest =
            if highest = pindex - 5
              then highest + 1
              else highest
          in
          draw_menu_entries ren entries new_pindex new_highest;
          refresh ren;
          loop new_pindex new_highest)
        else
          loop pindex highest

      | Return
      | KP_Enter -> pindex

      | _ -> loop pindex highest

      (* To do: Handle window resizing / closing *)
  in
  
  loop 0 0
;;
