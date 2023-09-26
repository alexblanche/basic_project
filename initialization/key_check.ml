(** Key check loop (executed in parallel) **)

(* Kill switch to exit key_check (closes the parallel execution of the domain) *)
(* Also serves to indicate that the window was closed *)
let exit_key_check = ref false;;
(* Resizing counter *)
let cpt_resize = ref 0;;

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


(* Key check loop *)
let rec key_check () =
  if not !exit_key_check then
    match Sdlevent.poll_event () with
    | None -> key_check ()
	
    (* Quitting *)
    | Some (Window_Event {kind = WindowEvent_Close})
    | Some (KeyDown {keycode = Escape}) -> raise Window_Closed

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
      (getkey := get_getkey_val key;
      key_pressed := key;
      key_check ())

    | Some (KeyUp {keycode = key}) ->
      (if key = !key_pressed then
        (getkey := 0;
        key_pressed := Unknown);
      key_check ())
  
    | _ -> key_check ()
;;

(* Launches the check loop *)
let launch_key_check () =
  (* Domain.at_exit (fun () -> print_endline "!!! Key check interruption !!!"); *)
  try
    key_check ()
  with
    | Window_Closed -> exit_key_check := true
;;