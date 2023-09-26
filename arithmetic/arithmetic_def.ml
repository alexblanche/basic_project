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
(* All functions take a list of arguments as parameter.
  This allows for functions of all arities and variable arity. *)

(* Some functions have their real and complex cases treated separately
  to reduce the rounding errors of complex operations *)

(* Hash table containing all the arithmetic functions *)
let func_table =
  let t = Hashtbl.create 10 in
  let (func_list : (string * (complex list -> complex)) list) = [
    (* Max: Works for complex numbers, as max of a pair in lexicographic order *)
    ("MAX",
      (let f l =
        let rec aux m l =
          match l with
            | x::t -> aux (max m x) t
            | [] -> m
        in
        match l with
          | a::t -> aux a t
          | [] -> failwith "Function error: Max should have at least one argument"
      in f)
    );

    ("ABS",
      (let f (l : complex list) =
        match l with
          | [z] ->
            if z.im = 0.
              then complex_of_float (Float.abs z.re)
              else complex_of_float (Complex.norm z)
          | _ -> failwith "Function error: Abs has arity 1"
      in f)
    );

    ("UMINUS",
      (let f (l : complex list) : complex =
        match l with
          | [z] -> {re = -.z.re; im = -.z.im}
          | _ -> failwith "Function error: Unary minus has arity 1"
      in f)
    );

    ("EPOWER",
      (let f (l : complex list) =
        match l with
          | [z] ->
            if z.im = 0.
              then complex_of_float (Float.exp z.re)
              else Complex.exp z
          | _ -> failwith "Function error: Abs has arity 1"
      in f)
    );

    ("NOT",
      (let f (l : complex list) : complex =
        match l with
          | [z] ->
            if z.im = 0.
              then complex_of_bool (z.re = 0.)
              else failwith "Function application error: Not accepts only real arguments"
          | _ -> failwith "Function error: Not has arity 1"
      in f)
    )
    ]
  in
  List.iter (fun (fname, fdef) -> Hashtbl.add t fname fdef) func_list;
  t;;

(** Application functions **)

(* Application of the functions *)
let apply_func (fname : string) (zl : complex list) =
  try
    (Hashtbl.find func_table fname) zl
  with
    | Not_found -> failwith ("apply_func: Function "^fname^" undefined");;

(* List of handled left unary operators *)
(* Temporary *)
let lop_list = ["ABS"; "UMINUS"; "EPOWER"];;

(* List of handled right unary operators *)
(* Temporary *)
let rop_list = ["EXCLAMATIONMARK"; "POWER2"];;

(* List of handled operators and their index of precedence (1 = greatest *)
let op_list = [("PLUS", 3); ("MINUS", 3); ("TIMES", 2); ("DIVIDED", 2); ("POWER", 1);
  ("LEQ", 4); ("LESS", 4); ("GEQ", 4); ("GREATER", 4); ("EQUAL", 4); ("DIFFERENT", 4);
  ("AND", 5); ("OR", 6); ("XOR", 7)];;



(* Application of the operators *)
let apply_op (o : string) (z1 : complex) (z2 : complex) : complex =
  (* Arithmetic *)
  if o = "PLUS" then Complex.add z1 z2
  else if o = "MINUS" then Complex.sub z1 z2
  else if o = "TIMES" then
    if z1.im = 0. && z2.im = 0.
      then complex_of_float (z1.re *. z2.re)
      else Complex.mul z1 z2
  else if o = "DIVIDED" then
    if z1.im = 0. && z2.im = 0.
      then complex_of_float (z1.re /. z2.re)
      else Complex.div z1 z2
  else if o = "POWER" then
    if z1.im = 0. && z2.im = 0.
      then complex_of_float (z1.re ** z2.re)
      else Complex.pow z1 z2
  (* Relations *)
  else if o = "LEQ" then complex_of_bool (z1 <= z2)
  else if o = "LESS" then complex_of_bool (z1 < z2)
  else if o = "GEQ" then complex_of_bool (z1 >= z2)
  else if o = "GREATER" then complex_of_bool (z1 > z2)
  else if o = "EQUAL" then complex_of_bool (is_zero_float (z1.re -. z2.re) && is_zero_float (z1.im -. z2.im))
  else if o = "DIFFERENT" then complex_of_bool (not (is_zero_float (z1.re -. z2.re)) || not (is_zero_float (z1.im -. z2.im)))
  (* Logic *)
  else if o = "AND" then complex_of_bool ((is_not_zero z1) && (is_not_zero z2))
  else if o = "OR" then complex_of_bool ((is_not_zero z1) || (is_not_zero z2))
  else if o = "XOR" then
    complex_of_bool ((is_not_zero z1) && (is_zero z2) || (is_zero z1) && (is_not_zero z2))
  else failwith ("apply_op: Unkown operator "^o);;

(* Application of the right unary operators *)
(* Since there are only a few, we hard-code them like the operators. *)
let apply_rop (ro : string) (z : complex) : complex =
  match ro with
    | "EXCLAMATIONMARK" -> complex_of_int (fact (int_of_float z.re))
    | "POWER2" -> Complex.mul z z
    | _ -> failwith ("apply_rop: Unkown operator "^ro);;

(* Application of the left unary operators *)
let apply_lop (lo : string) (z : complex) : complex =
  apply_func lo [z];;


(** prototype for complex arithmetic combined with list/matrix arithmetic **)
let 