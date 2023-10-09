(* Special functions definition *)
(* For these functions, not all arguments are evaluated before passing them to the function. *)

(* List of the special functions *)
(* For each function, the associated list gives the arity and indicates to the
  arithmetic expression evaluator which arguments should be evaluated beforehand (true)
  or passed as-is (false) *)
let special_functions_list =
  [("SEQ", [false; false; true; true; true]);
   ("PXLTEST", [true; true])];;

