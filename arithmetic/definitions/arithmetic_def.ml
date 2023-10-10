(* Definition of all the arithmetic functions of the Basic Casio language *)

(* Definition of functions not present in the standard library *)

(* Factorial *)
let fact n =
  let rec aux a i =
    if i = 0
      then a
      else aux (i*a) (i-1)
  in
  aux 1 n;;

(* Greatest common divisor *)
let rec gcd (a : int) (b : int) : int =
  if b = 0
    then a
    else gcd b (a mod b);;

(* Version of gcd for >= 2 integers *)
let gcd_l (l : int list) : int =
  match l with
    | x::y::t ->
      List.fold_left gcd x (y::t)
    | _ -> failwith "gcd_l: at least two integers are expected";;

(* Least common multiple *)
let lcm (a : int) (b : int) : int =
  (a*b)/(gcd a b);;

(* Version of lcm for >= 2 integers *)
let lcm_l (l : int list) : int =
  match l with
    | x::y::t ->
      List.fold_left lcm x (y::t)
    | _ -> failwith "lcm_l: at least two integers are expected"

(**********************************************************************************)


(* Table of handled functions *)
(* All functions take a list of arguments as parameter.
  This allows for functions of all arities and variable arity. *)
(* These functions must be closed by a parenthesis (RPAR),
   unless they are also in lop_list *)

(* Some functions have their real and complex cases treated separately
  to reduce the rounding errors of complex operations *)

(* Hash table containing all the arithmetic functions *)
let func_table =
  let (func_list : (string * (complex list -> complex)) list) = [
    (* Max: Works for complex numbers, as max of a pair in lexicographic order *)
    (* ("MAX",
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
    ); *)

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

    (* Unary minus *)
    ("UMINUS",
      (let f (l : complex list) : complex =
        match l with
          | [z] -> {re = -.z.re; im = -.z.im}
          | _ -> failwith "Function error: Unary minus has arity 1"
      in f)
    );

    (* e^ *)
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

    (* 10^ *)
    (* 10^(a+ib) = (10^a)(10^(ib)) = (10^a)(e^(i*b*ln 10)) *)
    ("TENPOWER",
      (let f (l : complex list) =
        match l with
          | [z] ->
            if z.im = 0.
              then complex_of_float (10. ** z.re)
              else scal (10. ** z.re) (Complex.exp (get_complex 0. (z.im*.Float.log 10.)))
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
    );

    ("MOD",
      (let f (l : complex list) : complex =
        match l with
          | [z1; z2] ->
            if is_int z1 && is_int z2 then
              let i1 = true_int_of_float z1.re in
              let i2 = true_int_of_float z2.re in
              if i1 >= 0
                then complex_of_int (i1 mod i2)
                else complex_of_int (i2 + (i1 mod i2))
            else failwith "Function application error: Mod accepts only integer arguments"
          | _ -> failwith "Function error: Mod has arity 2"
      in f)
    );

    ("INT",
      (let f (l : complex list) =
        match l with
          | [z] ->
            if z.im = 0.
              then
                complex_of_int (true_int_of_float z.re)
              else
                {re = float_of_int (true_int_of_float z.re);
                 im = float_of_int (true_int_of_float z.im)}
          | _ -> failwith "Function error: Int has arity 1"
      in f)
    );

    ("INTG",
      (let f (l : complex list) =
        match l with
          | [z] ->
            let intg x =
              if x >= 0.
                then float_of_int (true_int_of_float x)
                else float_of_int (true_int_of_float x - 1)
            in
            if z.im = 0.
              then complex_of_float (intg z.re)
              else
                {re = intg z.re;
                 im = intg z.im}
          | _ -> failwith "Function error: Intg has arity 1"
      in f)
    );

    ("FRAC",
      (let f (l : complex list) =
        match l with
          | [z] ->
            if z.im = 0.
              then complex_of_float (z.re -. float_of_int (true_int_of_float z.re))
              else
                {re = z.re -. float_of_int (true_int_of_float z.re);
                 im = z.im -. float_of_int (true_int_of_float z.im)}
          | _ -> failwith "Function error: Frac has arity 1"
      in f)
    );

    (* Real part *)
    ("REP",
      (let f (l : complex list) =
        match l with
          | [z] -> complex_of_float z.re
          | _ -> failwith "Function error: ReP has arity 1"
      in f)
    );

    (* Imaginary part *)
    ("IMP",
      (let f (l : complex list) =
        match l with
          | [z] -> complex_of_float z.im
          | _ -> failwith "Function error: ImP has arity 1"
      in f)
    );

    (* Trigonometric functions are only implemented in radians so far *)
    ("SIN",
      (let f (l : complex list) =
        match l with
          | [z] ->
            if z.im = 0.
              then complex_of_float (Float.sin z.re)
              else failwith "Function error: Sin only accepts real arguments"
          | _ -> failwith "Function error: Sin has arity 1"
      in f)
    );

    ("COS",
      (let f (l : complex list) =
        match l with
          | [z] ->
            if z.im = 0.
              then complex_of_float (Float.cos z.re)
              else failwith "Function error: Cos only accepts real arguments"
          | _ -> failwith "Function error: Cos has arity 1"
      in f)
    );

    ("TAN",
      (let f (l : complex list) =
        match l with
          | [z] ->
            if z.im = 0.
              then complex_of_float (Float.tan z.re)
              else failwith "Function error: Tan only accepts real arguments"
          | _ -> failwith "Function error: Tan has arity 1"
      in f)
    );

    ("ARCSIN",
      (let f (l : complex list) =
        match l with
          | [z] ->
            if z.im = 0.
              then complex_of_float (Float.asin z.re)
              else failwith "Function error: Sin only accepts real arguments"
          | _ -> failwith "Function error: Sin has arity 1"
      in f)
    );

    ("ARCCOS",
      (let f (l : complex list) =
        match l with
          | [z] ->
            if z.im = 0.
              then complex_of_float (Float.acos z.re)
              else failwith "Function error: Cos only accepts real arguments"
          | _ -> failwith "Function error: Cos has arity 1"
      in f)
    );

    ("ARCTAN",
      (let f (l : complex list) =
        match l with
          | [z] ->
            if z.im = 0.
              then complex_of_float (Float.atan z.re)
              else failwith "Function error: Tan only accepts real arguments"
          | _ -> failwith "Function error: Tan has arity 1"
      in f)
    );

    (* Natural logarithm (in base e) *)
    ("LN",
      (let f (l : complex list) =
        match l with
          | [z] -> Complex.log z
          | _ -> failwith "Function error: Ln has arity 1"
      in f)
    );

    (* Logarithm in base 10 *)
    ("LOG",
      (let f (l : complex list) =
        match l with
          | [z] ->
            if z.im = 0.
              then complex_of_float ((Float.log z.re) /. (Float.log 10.))
              else scal (1./.Float.log 10.) (Complex.log z)
          | _ -> failwith "Function error: Log has arity 1"
      in f)
    );

    ("SQRT",
      (let f (l : complex list) =
        match l with
          | [z] -> Complex.sqrt z
          | _ -> failwith "Function error: Sqrt has arity 1"
      in f)
    );

    (* Cubic root *)
    ("CURT",
      (let f (l : complex list) =
        match l with
          | [z] ->
            if z.im = 0.
              then complex_of_float (Float.pow z.re (1./.3.))
              else Complex.pow z (complex_of_float (1./.3.))
          | _ -> failwith "Function error: Sqrt has arity 1"
      in f)
    );

    (* n-th root *)
    ("NSQRT",
      (let f (l : complex list) =
        match l with
          | [z; n] ->
            if z.im = 0. && is_int n
              then complex_of_float (Float.pow z.re (1./. n.re))
              else Complex.pow z (complex_of_float (1./.n.re))
          | _ -> failwith "Function error: Nsqrt has arity 2"
      in f)
    );

    (* Hyperbolic functions *)
    ("SINH",
      (let f (l : complex list) =
        match l with
          | [z] ->
            if z.im = 0.
              then complex_of_float (Float.sinh z.re)
              else failwith "Function error: Sin only accepts real arguments"
          | _ -> failwith "Function error: Sinh has arity 1"
      in f)
    );

    ("COSH",
      (let f (l : complex list) =
        match l with
          | [z] ->
            if z.im = 0.
              then complex_of_float (Float.cosh z.re)
              else failwith "Function error: Cos only accepts real arguments"
          | _ -> failwith "Function error: Cosh has arity 1"
      in f)
    );

    ("TANH",
      (let f (l : complex list) =
        match l with
          | [z] ->
            if z.im = 0.
              then complex_of_float (Float.tanh z.re)
              else failwith "Function error: Tan only accepts real arguments"
          | _ -> failwith "Function error: Tanh has arity 1"
      in f)
    );

    ("ARCSINH",
      (let f (l : complex list) =
        match l with
          | [z] ->
            if z.im = 0.
              then complex_of_float (Float.asinh z.re)
              else failwith "Function error: Sin only accepts real arguments"
          | _ -> failwith "Function error: Arcsinh has arity 1"
      in f)
    );

    ("ARCCOSH",
      (let f (l : complex list) =
        match l with
          | [z] ->
            if z.im = 0.
              then complex_of_float (Float.acosh z.re)
              else failwith "Function error: Cos only accepts real arguments"
          | _ -> failwith "Function error: Arccosh has arity 1"
      in f)
    );

    ("ARCTANH",
      (let f (l : complex list) =
        match l with
          | [z] ->
            if z.im = 0.
              then complex_of_float (Float.atanh z.re)
              else failwith "Function error: Tan only accepts real arguments"
          | _ -> failwith "Function error: Arctanh has arity 1"
      in f)
    );

    (* Argument of a complex *)
    (* In radians so far *)
    ("ARG",
      (let f (l : complex list) =
        match l with
          | [z] -> complex_of_float (Complex.arg z)
          | _ -> failwith "Function error: Conjg has arity 1"
      in f)
    );

    (* Conjugate *)
    ("CONJG",
      (let f (l : complex list) =
        match l with
          | [z] -> Complex.conj z
          | _ -> failwith "Function error: Conjg has arity 1"
      in f)
    );

    ("GCD",
      (let f (l : complex list) =
        if List.length l < 2 || List.exists (fun z -> not (is_int z)) l
          then failwith "Function error: GCD has arity 2 and only accepts integer arguments"
        else complex_of_int (gcd_l (List.map int_of_complex l))
      in f)
    );

    ("LCM",
      (let f (l : complex list) =
        if List.length l < 2 || List.exists (fun z -> not (is_int z)) l
          then failwith "Function error: LCM has arity 2 and only accepts integer arguments"
        else complex_of_int (lcm_l (List.map int_of_complex l))
      in f)
    );
    ]
  in
  let t = Hashtbl.create (5 + List.length func_list) in
  List.iter (fun (fname, fdef) -> Hashtbl.add t fname fdef) func_list;
  t;;

(* List of handled left unary operators *)
let lop_list = [
  "ABS"; "NOT"; "UMINUS"; "EPOWER"; "TENPOWER"; "INT"; "INTG"; "FRAC";
  "REP"; "IMP"; "ARG";
  "LN"; "LOG"; "SQRT";
  "SIN"; "COS"; "TAN"; "ARCSIN"; "ARCCOS"; "ARCTAN";
  "SINH"; "COSH"; "TAN"; "ARCSINH"; "ARCCOSH"; "ARCTANH";
  "CURT"; "NSQRT"; "CONJG"
  ];;

(* List of handled right unary operators *)
let rop_list = [
  "EXCLAMATIONMARK"; "POWER2"; "POWERMINUS1";
  "FEMTO"; "PICO"; "NANO"; "MICRO"; "MILLI";
  "KILO"; "MEGA"; "GIGA"; "TERA"; "PETA"; "EXA"
  ];;

(* List of handled operators and their index of precedence (1 = greatest *)
(* The operators are ordered by frequency in usual expresssions *)
let op_list = [
  ("PLUS", 3); ("MINUS", 3);
  ("TIMES", 2); ("DIVIDED", 2);
  ("POWER", 0);
  ("LEQ", 4); ("LESS", 4); ("GEQ", 4); ("GREATER", 4); ("EQUAL", 4); ("DIFFERENT", 4);
  ("AND", 5);
  ("OR", 6);
  ("INTDIV", 1); ("RMDR", 1);
  ("XOR", 6)
  ];;

(* All precedences were checked on a Casio calculator
        POWER
    << (INTDIV, RMDR)
    << (TIMES, DIVIDED)
    << (PLUS, MINUS)
    << (LEQ, LESS, GEQ, GREATER, EQUAL, DIFFERENT)
    << AND
    << (OR, XOR)
*)


(* Application of the functions *)
let apply_func (fname : string) (zl : complex list) : complex =
  try
    (Hashtbl.find func_table fname) zl
  with
    | Not_found -> failwith ("apply_func: Function "^fname^" undefined");;

(** Application of operators and functions to one complex **)

(* Real and complex operations are sometimes separated for efficiency and precision
  (ex: float ** is much more precise than Complex.pow) *)
let apply_op_single (o : string) (z1 : complex) (z2 : complex) : complex =
  match o with
  (* Arithmetic *)
    | "PLUS" -> Complex.add z1 z2
    | "MINUS" -> Complex.sub z1 z2
    | "TIMES" ->
      if z1.im = 0. && z2.im = 0.
        then complex_of_float (z1.re *. z2.re)
        else Complex.mul z1 z2
    | "DIVIDED" ->
      if z1.im = 0. && z2.im = 0.
        then complex_of_float (z1.re /. z2.re)
        else Complex.div z1 z2
    | "POWER" ->
      if z1.im = 0. && z2.im = 0.
        then complex_of_float (z1.re ** z2.re)
        else Complex.pow z1 z2
  (* Relations *)
    | "LEQ" -> complex_of_bool (z1 <= z2)
    | "LESS" -> complex_of_bool (z1 < z2)
    | "GEQ" -> complex_of_bool (z1 >= z2)
    | "GREATER" -> complex_of_bool (z1 > z2)
    | "EQUAL" ->
      complex_of_bool (is_zero_float (z1.re -. z2.re) && is_zero_float (z1.im -. z2.im))
    | "DIFFERENT" ->
      complex_of_bool (not (is_zero_float (z1.re -. z2.re)) || not (is_zero_float (z1.im -. z2.im)))
  (* Logic *)
    | "AND" -> complex_of_bool ((is_not_zero z1) && (is_not_zero z2))
    | "OR" -> complex_of_bool ((is_not_zero z1) || (is_not_zero z2))
    | "XOR" ->
      complex_of_bool ((is_not_zero z1) && (is_zero z2) || (is_zero z1) && (is_not_zero z2))
  (* Others *)

    (* /!\
      (-27) intdiv 5 = -5 (as in -27 = -5*5-2)
      (-27) rmdr   5 = 3  (as in -27 = -6*5+3)
    *)
    | "INTDIV" ->
      if is_int z1 && is_int z2 then
        let i1 = true_int_of_float z1.re in
        let i2 = true_int_of_float z2.re in
        complex_of_int (i1 / i2)
      else failwith "apply_op: Intdiv only accepts integer arguments"
    | "RMDR" ->
      if is_int z1 && is_int z2 then
        let i1 = true_int_of_float z1.re in
        let i2 = true_int_of_float z2.re in
        if i1 >= 0 (* tested for both signs i1, i2 *)
          then complex_of_int (i1 mod i2)
          else complex_of_int ((abs i2) + (i1 mod i2))
      else failwith "apply_op: Rmdr only accepts integer arguments"

    | _ -> failwith ("apply_op: Unkown operator "^o);;

(* Application of the right unary operators *)
(* Since there are only a few, we hard-code them like the operators. *)
let apply_rop_single (ro : string) (z : complex) : complex =
  match ro with
    | "EXCLAMATIONMARK" ->
      if is_int z
        then complex_of_int (fact (true_int_of_float z.re))
        else failwith "apply_rop: Factorial only accepts integer arguments"
    | "POWER2" ->
      if z.im = 0.
        then complex_of_float (z.re *. z.re)
        else Complex.mul z z
    | "POWERMINUS1" ->
      if z.im = 0.
        then complex_of_float (1. /. z.re)
        else Complex.div {re = 1.; im = 0.} z
    | "FEMTO" ->
      if z.im = 0.
        then complex_of_float (z.re *. 1e-15)
        else scal 1e-15 z
    | "PICO" ->
      if z.im = 0.
        then complex_of_float (z.re *. 1e-12)
        else scal 1e-12 z
    | "NANO" ->
      if z.im = 0.
        then complex_of_float (z.re *. 1e-09)
        else scal 1e-09 z
    | "MICRO" ->
      if z.im = 0.
        then complex_of_float (z.re *. 1e-06)
        else scal 1e-06 z
    | "MILLI" ->
      if z.im = 0.
        then complex_of_float (z.re *. 1e-03)
        else scal 1e-03 z
    | "KILO" ->
      if z.im = 0.
        then complex_of_float (z.re *. 1000.)
        else scal 1000. z
    | "MEGA" ->
      if z.im = 0.
        then complex_of_float (z.re *. 1000000.)
        else scal 1000000. z
    | "GIGA" ->
      if z.im = 0.
        then complex_of_float (z.re *. 1e09)
        else scal 1e09 z
    | "TERA" ->
      if z.im = 0.
        then complex_of_float (z.re *. 1e12)
        else scal 1e12 z
    | "PETA" ->
      if z.im = 0.
        then complex_of_float (z.re *. 1e15)
        else scal 1e15 z
    | "EXA" ->
      if z.im = 0.
      then complex_of_float (z.re *. 1e18)
      else scal 1e18 z
    | _ -> failwith ("apply_rop: Unkown operator "^ro);;

(* Application of the left unary operators *)
let apply_lop_single (lo : string) (z : complex) : complex =
  apply_func lo [z];;

