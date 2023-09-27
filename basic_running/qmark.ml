(** Executes the ? (QMark) operation **)

let qmark (win : Sdlwindow.t) (ren : Sdlrender.t) : basic_expr * expression_type =
  (* Main loop *)
  (* ns: list of strings storing the number entered
    x: index of writing in line !writing_index *)
  let rec loop (ns : string list) (x : int) : string list =
    
    wait_press ren true;
    
    let key = !key_pressed in
    (* Enter or keypad Enter *)
    if (key = Return || key = KP_Enter) && ns <> []
      then List.rev ns
    else if key = Escape
      then
        if ns <> []
          then (* Something was typed: Reset *)
            (clear_text ();
            locate_no_refresh ["?"] 0 0;
            writing_index := 1;
            tdraw ren;
            wait_release ren true;
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
          wait_release ren true;
          loop (cs::ns) 1)
        else
          (locate ren [cs] x !writing_index;
          wait_release ren true;
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
          wait_release ren true;
          loop (lex::ns) ((x + len) mod 21))
        else
          (locate ren cs x !writing_index;
          wait_release ren true;
          loop (lex::ns) (x+len))
    else
      (if !parameters_updated then
        tdraw ren
      else if !exit_key_check then
        raise Window_Closed;
      loop ns x)
  in

  line_feed ();
  clear_line !writing_index;
  locate_no_refresh ["?"] 0 !writing_index;
  line_feed ();
  tdraw ren;
  (* The writing line is not cleared on screen until a character is typed *)
  clear_line !writing_index;

  escape_activated := false;
  let ns = loop [] 0 in
  escape_activated := true;
  
  let (e, expr_type, t) = extract_expr ns in
  if t <> []
    then failwith "Runtime error: wrong entry";
  (e, expr_type);;
