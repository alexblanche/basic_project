(** Key check loop (executed in parallel) **)

(* Kill switch to exit key_check (closes the parallel execution of the domain) *)
(* Also serves to indicate that the window was closed *)
let exit_key_check = ref false;;
(* Resizing counter *)
let cpt_resize = ref 0;;

(* Exception to the activation of the kill switch via an Escape key press *)
(* True if Escape interrupts the program, False if it proceeds as normal *)
let escape_activated = ref true;;

(* Contains the Getkey value *)
let getkey = ref 0;;
(* Contains the key pressed (type Sdlkeycode.t) *)
let key_pressed = ref Sdlkeycode.Unknown;;


(* Getkey values *)
(* Returns the Getkey value of the given key, as observerd on a Casio calculator *)
(* Value = 10*a + b
b\a  7     6    5    4    3    2
9    F1    F2   F3   F4   F5   F6
8    SHIFT OPTN VARS MENU <    ^
7    ALPHA x2   ^    EXIT v    >
6    XthT  log  ln   sin  cos  tan
5    FRAC  FD   (    )    ,    ->
4    7     8    9    DEL  (AC)
3    4     5    6    x     /
2    1     2    3    +     -
1    0     .    E    (-)   EXE
*)
(* AC = 34 is treated separately (not an actual Getkey value) *)
let get_getkey_val (key : Sdlkeycode.t) : int =
  try
    List.assoc key
    (* To do: toggle Qwerty mode *)
    [
      (F1, 79); (F2, 69); (F3, 59); (F4, 49); (F5, 39); (F6, 29);
      (LShift, 78); (* ('\000', 68); ('\000', 58); *) (RShift, 48); (Left, 38); (Up, 28);
      (LCtrl, 77); (* ('\000', 67); *) (Power, 57); (Delete, 47); (Down, 37); (Right, 27);
      (A, 76); (B, 66); (C, 56); (D, 46); (E, 36); (F, 26);
      (G, 75); (H, 65); (I, 55); (LeftParen, 55); (J, 45); (RightParen, 45); (K, 35); (KP_Comma, 35); (L, 25);
      (KP_7, 74); (KP_8, 64); (KP_9, 54); (Backspace, 44);
      (KP_4, 73); (KP_5, 63); (KP_6, 53); (KP_Multiply, 43); (KP_Divide, 33);
      (KP_1, 72); (KP_2, 62); (KP_3, 52); (KP_Plus, 42); (KP_Minus, 32);
      (KP_0, 71); (KP_Period, 61); (Num3, 51); (Num6, 41); (KP_Enter, 31); (Return, 31);

      (M, 74); (N, 64); (O, 54);
      (P, 73); (Q, 63); (R, 53); (S, 43); (T, 33);
      (U, 72); (V, 62); (W, 52); (X, 42); (Y, 32);
      (Z, 71); (Space, 61); (KP_Space, 61); 
    ]
  with
    | Not_found -> 0;;


(* Key check loop *)
let rec key_check () =
  if not !exit_key_check then
    match Sdlevent.poll_event () with
    | None -> key_check ()
	
    (* Quitting *)
    | Some (Window_Event {kind = WindowEvent_Close}) -> raise Window_Closed
    | Some (KeyDown {keycode = Escape}) ->
      if !escape_activated
        then raise Window_Closed
        else
          (getkey := 34;
          key_pressed := Escape;
          key_check ())

    (* Resizing *)
    | Some (Window_Event {kind = WindowEvent_Resized wxy}) ->
      (incr cpt_resize;
      if !cpt_resize >= resize_threshold then
        (cpt_resize := 0;
        update_parameters wxy.win_x wxy.win_y);
      key_check ())

    | Some (Window_Event {kind = WindowEvent_Exposed})
    | Some Keymap_Changed ->
      (cpt_resize := resize_threshold;
      key_check ())
        
    (* KeyDown, KeyUp check *)
    | Some (KeyDown {keycode = key}) ->
      (* (if !getkey = 0 then
        (getkey := get_getkey_val key;
        key_pressed := key);
      key_check ()) *)
      (getkey := get_getkey_val key;
      key_pressed := key;
      key_check ())


    | Some (KeyUp {keycode = key}) ->
      (if key = !key_pressed then
        (getkey := 0;
        key_pressed := Unknown);
      key_check ())
    
    (* | Some Controller_Button_Down -> (print_endline "Controller button down"; key_check ())
    | Some (Joy_Button_Down _) -> (print_endline "Joy button down"; key_check ())
    | Some (Joy_Axis_Motion _) -> (print_endline "Joy axis motion"; key_check ())
    | Some Controller_Axis_Motion -> (print_endline "Controller axis motion"; key_check ())
    | Some (Mouse_Button_Down _) -> (print_endline "Mouse button down"; key_check ())
    | Some (Mouse_Motion _) -> (print_endline "..."; key_check ())
    | Some (Joy_Device_Added _) -> (print_endline "Joy device added"; key_check ())
    |	Some Controller_Device_Added -> (print_endline "Controller device added"; key_check ())
    |	Some Controller_Device_Removed -> (print_endline "Controller device removed"; key_check ()) *)

    | _ -> key_check ()
;;

(* Launches the check loop *)
let launch_key_check () =
  (* Domain.at_exit (fun () -> print_endline "Key check interruption"); *)
  try
    key_check ()
  with
    | Window_Closed ->
      exit_key_check := true
;;