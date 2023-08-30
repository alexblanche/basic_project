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

(* All numbers are complex numbers, of type Complex.t *)



(* Temporary example *)
let func_table =
  let t = Hashtbl.create 10 in
  let func_list = [
    ("max", AR2 (fun a b -> max a b)); (* Works for complex numbers, as max of a pair in lexicographic order *)
    ("f", AR1 (fun z -> Complex.add (Complex.mul z z) {re = 2.; im = 0.}));
    ("max3", AR3 (fun a b c -> max (max a b) c));
    ("fact", AR1 (fun (z : Complex.t) -> complex_of_int (fact (int_of_float z.re))));
    ("Abs", LOP (fun z -> complex_of_float (Complex.norm z)))
    ]
  in
  List.iter (fun (fname, fdef) -> Hashtbl.add t fname fdef) func_list;
  t;;

(** Application functions **)

(* Application of the functions *)
let apply_func (fname : string) (zl : complex list) =
  try
    (match (Hashtbl.find func_table fname, zl) with
      | LOP f, [z1] -> f z1
      | LOP _, _ -> failwith ("apply_func: Operator "^fname^" has a wrong number of arguments")
      | AR1 f, [z1] -> f z1
      | AR2 f, [z1;z2] -> f z1 z2
      | AR3 f, [z1;z2;z3] -> f z1 z2 z3
      | AR4 f, [z1;z2;z3;z4] -> f z1 z2 z3 z4
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



(* Application of the operators *)
let apply_op (o : string) (z1 : complex) (z2 : complex) : complex =
  (* Arithmetic *)
  if o = "PLUS" then Complex.add z1 z2
  else if o = "MINUS" then Complex.sub z1 z2
  else if o = "TIMES" then Complex.mul z1 z2
  else if o = "DIVIDED" then Complex.div z1 z2
  else if o = "POWER" then Complex.pow z1 z2
  (* Relations *)
  else if o = "LEQ" then complex_of_bool (z1 <= z2)
  else if o = "LESS" then complex_of_bool (z1 < z2)
  else if o = "GEQ" then complex_of_bool (z1 >= z2)
  else if o = "GREATER" then complex_of_bool (z1 > z2)
  else if o = "EQUAL" then complex_of_bool (z1 = z2)
  else if o = "DIFFERENT" then complex_of_bool (z1 <> z2)
  (* Logic *)
  else if o = "AND" then complex_of_bool ((is_not_zero z1) && (is_not_zero z2))
  else if o = "OR" then complex_of_bool ((is_not_zero z1) || (is_not_zero z2))
  else if o = "XOR" then
    complex_of_bool ((is_not_zero z1) && (is_zero z2) || (is_zero z1) && (is_not_zero z2))
  else failwith ("apply_op: Unkown operator "^o);;

(* Application of the right unary operators *)
(* Since there are only a few, we hard-code them like the operators. *)
let apply_rop (ro : string) (z : complex) : complex =
  if ro = "!"
    then complex_of_int (fact (int_of_float z.re))
    else failwith ("apply_rop: Unkown operator "^ro);;

(* Application of the left unary operators *)
let apply_lop (lo : string) (z : complex) : complex =
  apply_func lo [z];;