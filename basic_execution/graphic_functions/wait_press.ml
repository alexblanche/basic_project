(* Waiting for a key to be pressed *)

(* Waits for a key to be pressed (or released if getkey_value is 0) *)
(* Raises exception Window_Closed if the window is closed or Escape is pressed *)
(* When the window is resized, if text_screen is true, tdraw is applied, otherwise gdraw *)
  let wait_press_key (ren : Sdlrender.t) (text_screen : bool) (getkey_value : int): unit =
    while !getkey <> getkey_value do
      if !parameters_updated then
        if text_screen
          then tdraw ren
          else gdraw ren
      else if !exit_key_check
        then raise Window_Closed
    done;;
  
  (* Same as the previous function, but wait for any key to be pressed *)
  let wait_press (ren : Sdlrender.t) (text_screen : bool) : unit =
    while !getkey = 0 do
      if !parameters_updated then
        if text_screen
          then tdraw ren
          else gdraw ren
      else if !exit_key_check
        then raise Window_Closed
    done;;
  
  (* Waits for Enter key to be pressed *)
  let wait_enter (ren : Sdlrender.t) (text_screen : bool) : unit =
    wait_press_key ren text_screen 31;;
  
  (* Wait for a key to be released *)
  let wait_release (ren : Sdlrender.t) (text_screen : bool) : unit =
    wait_press_key ren text_screen 0;;