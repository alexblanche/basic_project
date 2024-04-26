(* Runtime error functions *)

(* Raises an exception for runtime errors *)
let run_fail (i : int) (error_message : string) =
  failwith ("Runtime error at line "^(string_of_int i)^": "^error_message);;

(* Graphic commands errors *)
let graphic_fail (i : int) (error_message : string) =
  failwith ("Graphic error at line "^(string_of_int i)^": "^error_message);;