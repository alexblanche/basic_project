(* Definition of all the arithmetic functions of the Basic Casio language *)
(* #use "basic_parsing/basic_type.ml" *)

(* Factorial *)
let fact n =
  let rec aux a i =
    if i = 0
      then a
      else aux (i*a) (i-1)
  in
  aux 1 n;;

(* Table of handled functions *)
(* Each function is stored as a pair (a,f),
  where a is the arity of the function (number of arguments) *)

(* Temporary example *)
let func_table =
  let t = Hashtbl.create 10 in
  let func_list = [
    ("max", AR2 (fun a b -> max a b));
    ("f", AR1 (fun x -> x*.x+.2.));
    ("max3", AR3 (fun a b c -> max (max a b) c));
    ("fact", AR1 (fun x -> float_of_int (fact (int_of_float x))));
    ("Abs", LOP Float.abs)
    ]
  in
  List.iter (fun (fname, fdef) -> Hashtbl.add t fname fdef) func_list;
  t;;

(** Application functions **)

(* Application of the functions *)
let apply_func (fname : string) (xl : float list) =
  try
    (match (Hashtbl.find func_table fname, xl) with
      | LOP f, [x1] -> f x1
      | LOP _, _ -> failwith ("apply_func: Operator "^fname^" has a wrong number of arguments")
      | AR1 f, [x1] -> f x1
      | AR2 f, [x1;x2] -> f x1 x2
      | AR3 f, [x1;x2;x3] -> f x1 x2 x3
      | AR4 f, [x1;x2;x3;x4] -> f x1 x2 x3 x4
      | _ -> failwith ("apply_func: Function "^fname^" has a wrong number of arguments")
    )
  with
    | Not_found -> failwith ("apply_func: Function "^fname^" undefined");;

(* List of handled left unary operators *)
(* Temporary *)
let lop_list = ["Abs"];;

(* List of handled right unary operators *)
(* Temporary *)
let rop_list = ["!"];;

(* List of handled operators and their index of precedence (1 = greatest *)
let op_list = [("PLUS", 3); ("MINUS", 3); ("TIMES", 2); ("DIVIDED", 2); ("POWER", 1);
  ("LEQ", 4); ("LESS", 4); ("GEQ", 4); ("GREATER", 4); ("EQUAL", 4); ("DIFFERENT", 4);
  ("AND", 5); ("OR", 6); ("XOR", 7)];;




let float_of_bool (b : bool) : float =
  if b then 1. else 0.;;

let is_zero (x : float) : bool =
  x < 1e-15 && -.x < 1e-15;;

let is_not_zero (x : float) : bool =
  not (is_zero x);;

(* Application of the operators *)
let apply_op (o : string) (x1 : float) (x2 : float) : float =
  (* Arithmetic *)
  if o = "PLUS" then x1 +. x2
  else if o = "MINUS" then x1 -. x2
  else if o = "TIMES" then x1 *. x2
  else if o = "DIVIDED" then x1 /. x2
  else if o = "POWER" then x1 ** x2
  (* Relations *)
  else if o = "LEQ" then float_of_bool (x1 <= x2)
  else if o = "LESS" then float_of_bool (x1 < x2)
  else if o = "GEQ" then float_of_bool (x1 >= x2)
  else if o = "GREATER" then float_of_bool (x1 > x2)
  else if o = "EQUAL" then float_of_bool (x1 = x2)
  else if o = "DIFFERENT" then float_of_bool (x1 <> x2)
  (* Logic *)
  else if o = "AND" then float_of_bool ((is_not_zero x1) && (is_not_zero x2))
  else if o = "OR" then float_of_bool ((is_not_zero x1) || (is_not_zero x2))
  else if o = "XOR" then
    float_of_bool ((is_not_zero x1) && (is_zero x2) || (is_zero x1) && (is_not_zero x2))
  else failwith ("apply_op: Unkown operator "^o);;

(* Application of the right unary operators *)
(* Since there are only a few, we hard-code them like the operators. *)
let apply_rop (ro : string) (x : float) : float =
  if ro = "!"
    then float_of_int (fact (int_of_float x))
    else failwith ("apply_rop: Unkown operator "^ro);;

(* Application of the left unary operators *)
let apply_lop (lo : string) (x : float) : float =
  apply_func lo [x];;