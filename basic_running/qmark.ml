(** Executes the ? (QMark) operation **)

let qmark (win : Sdlwindow.t) (ren : Sdlrender.t) : basic_expr =
  (* Same as wait_keydown but supports resizing *)
  let rec wait_keydown_resize (cpt_resize : int) : Sdlkeycode.t =
    match Sdlevent.poll_event () with
      (* Quitting *)
      | Some (Window_Event {kind = WindowEvent_Close}) -> raise Window_Closed
      (* Resizing *)
			| Some (Window_Event {kind = WindowEvent_Resized wxy}) ->
        if cpt_resize >= resize_threshold then
					(update_parameters wxy.win_x wxy.win_y;
					tdraw ren;
          wait_keydown_resize 0)
        else wait_keydown_resize (cpt_resize + 1)
      
      | Some (KeyDown {keycode = keycode}) -> keycode
      | _ -> wait_keydown_resize cpt_resize
  in

  (* Main loop *)
  (* ns: list of strings storing the number entered
    x: index of writing in line !writing_index *)
  let rec loop (ns : string list) (x : int) : string list =
    let key = wait_keydown_resize 0 in
    flush_events ();
    (* Enter or keypad Enter *)
    if (key = Return || key = KP_Enter) && ns <> []
      then List.rev ns
    else if key = Escape
      then
        if ns <> []
          then (* Something was typed: Reset *)
            (clear_text ();
            locate ren ["?"] 0 0;
            writing_index := 1;
            wait_keyup key;
            loop [] 0)
          else (* Nothing was typed: Exitting the program *)
            raise Runtime_interruption
    else if List.mem key (* 0..9, '.': the symbol is the lexeme *)
        [KP_0; KP_1; KP_2; KP_3; KP_4; KP_5; KP_6; KP_7; KP_8; KP_9; KP_Period]
      then
        let cs = List.assoc key
          [(KP_0, "0"); (KP_1, "1"); (KP_2, "2"); (KP_3, "3"); (KP_4, "4");
          (KP_5, "5"); (KP_6, "6"); (KP_7, "7"); (KP_8, "8"); (KP_9, "9");
          (KP_Period, ".")]
        in
        if x = 21 then
          (line_feed ();
          locate_no_refresh [cs] 0 !writing_index;
          tdraw ren;
          wait_keyup key;
          loop (cs::ns) 1)
        else
          (locate ren [cs] x !writing_index;
          wait_keyup key;
          loop (cs::ns) (x+1))
    else if List.mem key (* Other lexemes: their representation is different from the lexeme *)
        [LeftParen; RightParen; KP_Plus; KP_Minus; KP_Multiply; KP_Divide]
      then
        let lex = List.assoc key
          [(LeftParen, "LPAR"); (RightParen, "RPAR");
          (KP_Plus, "PLUS"); (KP_Minus, "MINUS"); (KP_Multiply, "TIMES"); (KP_Divide, "DIVIDED")]
        in
        let repr = List.assoc lex text_display in (* String representing the function *)
        let cs = str_to_rev_symblist_full repr in (* string list printed by the Locate *)
        let len = List.length cs in
        if x + len >= 21 then
          (let (csk,csnk) = split_k cs (len-(21-x)) in
          locate_no_refresh (List.rev csnk) x !writing_index;
          line_feed ();
          locate_no_refresh (List.rev csk) 0 !writing_index;
          tdraw ren;
          wait_keyup key;
          loop (lex::ns) ((x + len) mod 21))
        else
          (locate ren cs x !writing_index;
          wait_keyup key;
          loop (lex::ns) (x+len))
    else
      loop ns x
  in

  line_feed ();
  clear_line !writing_index;
  locate_no_refresh ["?"] 0 !writing_index;
  line_feed ();
  tdraw ren;
  (* The writing line is not cleared on screen until a character is typed *)
  clear_line !writing_index;

  let ns = loop [] 0 in
  
  let (e,t) = extract_expr ns in
  if t <> []
    then failwith "Runtime error: wrong entry";
  e;;
