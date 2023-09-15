(** Getkey related **)

(* Equals true if the recorded key is released *)
let getkey_released = ref true;;

(* Getkey values *)
(* Returns the Getkey value of the given key, as observerd on a Casio calculator *)
(* Value = 10*a + b
b\a  7     6    5    4    3    2
9    F1    F2   F3   F4   F5   F6
8    SHIFT OPTN VARS MENU <    ^
7    ALPHA x2   ^    EXIT v    >
6    XthT  log  ln   sin  cos  tan
5    FRAC  FD   (    )    ,    ->
4    7     8    9    DEL
3    4     5    6    x     /
2    1     2    3    +     -
1    0     .    E    (-)   EXE
*)
let get_getkey_val (key : Sdlkeycode.t) : int =
  try
    List.assoc key
    (* To do: toggle Qwerty mode *)
    [
      (F1, 79); (F2, 69); (F3, 59); (F4, 49); (F5, 39); (F6, 29);
      (LShift, 78); (* ('\000', 68); ('\000', 58); ('\000', 48); *) (Left, 38); (Up, 28);
      (LCtrl, 77); (* ('\000', 67); *) (Power, 57); (Delete, 47); (Down, 37); (Right, 27);
      (* ('\000', 76); ('\000', 66); ('\000', 56); ('\000', 46); ('\000', 36); ('\000', 26); *)
      (* ('\000', 75); ('\000', 65); *) (LeftParen, 55); (RightParen, 45); (KP_Comma, 35); (* ('\000', 25); *)
      (KP_7, 74); (KP_8, 64); (KP_9, 54); (Backspace, 44);
      (KP_4, 73); (KP_5, 63); (KP_6, 53); (KP_Multiply, 43); (KP_Divide, 33);
      (KP_1, 72); (KP_2, 62); (KP_3, 52); (KP_Plus, 42); (KP_Minus, 32);
      (KP_0, 71); (KP_Period, 61); (* ('\000', 51); ('\000', 41); *) (KP_Enter, 31); (Return, 31)
    ]
  with
    | Not_found -> 0;;

(* Returns a keycode if a key is pressed, or None if 8300 None events have been encountered *)
(* If the current key is released, waits for the next KeyDown event and returns its keycode. *)
(* Experimentally, there are about 8000 "None" events between two KeyDown events when holding a key *)
(* Handles window resizing *)
let rec read_getkey_input (current_getkey : int) : (bool * Sdlkeycode.t option) =
  let rec aux (none_counter : int) =
    match Sdlevent.poll_event () with
      | None ->
        if none_counter < 8300
          then aux (none_counter + 1)
          else (false, None)

      (* Quitting *)
      | Some (Window_Event {kind = WindowEvent_Close})
      | Some (KeyDown {keycode = Escape}) -> raise Window_Closed

      (* Resizing *)
      | Some (Window_Event {kind = WindowEvent_Resized wxy}) ->
        (update_parameters wxy.win_x wxy.win_y;
        aux none_counter)
      
      (* Input *)
      | Some (KeyDown {keycode = key}) ->
        (false, Some key)

      | Some (KeyUp kue) ->
        if get_getkey_val (kue.keycode) = current_getkey
					then
            (getkey_released := true;
            let (_, k) = read_getkey_input 0 in
            (true, k))
					else aux none_counter

      | _ -> aux none_counter
  in
  aux 0;;