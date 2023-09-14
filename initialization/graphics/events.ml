(* Events handling *)

(* Loop that is required to make Sdlmouse.get_state work *)
let rec flush_events () : unit =
  match Sdlevent.poll_event () with
    | None -> ()
    | _ -> flush_events ();;

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