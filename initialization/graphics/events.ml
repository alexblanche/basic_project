(* Events handling *)
(* Used only in the picture edition interface, where the sequential event handling is still in place *)
(* In the emulator, the events are handled in parallel *)

(* Loop that is required to make Sdlmouse.get_state work *)
let rec flush_events () : unit =
  match Sdlevent.poll_event () with
    | None -> ()
    | _ -> flush_events ();;

(* Checks if the window was resized, closed or if Escape was pressed,
  until a None event is encountered *)
(* Return true if a KeyUp event was encountered, false otherwise *)
let rec check_resize_close () : bool =
  match Sdlevent.poll_event () with
	  | None -> false
	
	  (* Quitting *)
    | Some (Window_Event {kind = WindowEvent_Close})
    | Some (KeyDown {keycode = Escape}) -> raise Window_Closed

    (* Resizing *)
    | Some (Window_Event {kind = WindowEvent_Resized wxy}) ->
      (update_parameters wxy.win_x wxy.win_y; false)

    | Some (KeyUp _) -> true
	
    | _ -> check_resize_close ();;

(* Wait for KeyDown event, returns the keycode of the first key pressed *)
let rec wait_keydown () : Sdlkeycode.t =
  match Sdlevent.poll_event () with
    | Some (Window_Event {kind = WindowEvent_Close}) -> raise Window_Closed
    | Some (KeyDown {keycode = keycode}) -> keycode
    | _ -> wait_keydown ();;

(* Wait for KeyUp event with given keycode *)
let rec wait_keyup (keycode : Sdlkeycode.t) : unit =
  match Sdlevent.poll_event () with
    | Some (Window_Event {kind = WindowEvent_Close}) -> raise Window_Closed
    | Some (KeyUp {keycode = k}) ->
      if k = keycode
        then ()
        else wait_keyup keycode
    | _ -> wait_keyup keycode;;

