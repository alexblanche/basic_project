(* Functions related to complex numbers *)

type complex = Complex.t;;

(** Recoding the int_of_float conversion **)
(* Going through string_of_float to lose some precision, and
  avoid the 1.99999999999978 case *)

(*** In case it still does not work, I should recode all the
    operations manually with fractions ***)

let true_int_of_float (x : float) : int =
  let r = Float.rem x 1. in
  if r > 0.99999 || r < -0.99999
    then int_of_float (float_of_string (string_of_float x))
    else int_of_float x;;


(** Conversions **)

(* Returns the complex number x + i*y *)
let get_complex (x : float) (y : float) : complex =
  {re = x; im = y};;

(* Converts the float x into complex type *)
let complex_of_float (x : float) : complex =
  get_complex x 0.;;

(* Converts the int n into complex type *)
let complex_of_int (n : int) : complex =
  complex_of_float (float_of_int n);;

(* Converts the real part of complex z to an int *)
let int_of_complex (z : complex) : int =
  true_int_of_float z.re;;


(** Boolean evaluation **)

(* Converts a boolean to 1. if true and 0. if false *)
let float_of_bool (b : bool) : float =
  if b then 1. else 0.;;
  
let complex_of_bool (b : bool) : complex =
  complex_of_float (float_of_bool b);;
  
(* Returns true if the float x is close enough to 0.
    This accounts for imprecision inherent to float calculations. *)
let is_zero_float (x : float) : bool =
  x < 1e-13 && -.x < 1e-13;;

let are_equal_float (x1 : float) (x2 : float) : bool =
  let pow = true_int_of_float (Float.log10 x1) in
  let factor = 10. ** (float_of_int (14 - pow)) in
  Float.abs (x1 *. factor -. x2 *. factor) < 10000.;;

let are_equal (z1 : complex) (z2 : complex) : bool =
  (z1.re = z2.re || are_equal_float z1.re z2.re)
  && (z1.im = z2.im || are_equal_float z1.im z2.im);;
  
(* Returns true if the complex z has a real part close enough to 0.
  and an imaginary part equal to 0.
  It is motivated by the fact that boolean conditions do not involve imaginary parts. *)
let is_zero (z : complex) : bool =
  is_zero_float z.re && z.im = 0.;;
  
let is_not_zero (z : complex) : bool =
  not (is_zero z);;


(** Others **)

(* Returns true if the complex z represents an integer *)
(* Terrible precision allowed, because OCaml float calculations have an AWFUL precision *)
let is_int (z : complex) : bool =
  (let r = Float.rem z.re 1. in
  r <= 1e-08 || r > 0.99999999)
  &&
  z.im = 0.;;

(** Recoding of the operations **)

(* The exponentiation Complex.pow seems a lot less precise than the float one (**) on float numbers *)
(* let pow (z1 : complex) (z2 : complex) : complex =
  if z1.im = 0. && z2.im = 0.
    then get_complex (z1.re ** z2.re) 0.
    else Complex.pow z1 z2;; *)

let scal (a : float) (z : complex) : complex =
  get_complex (a *. z.re) (a *. z.im);;


(** Rounding **)

(* Diminishes the accuracy to 14 significant numbers *)
let round_float (x : float) : float =
  let pow = int_of_float (Float.log10 x) in
  let factor = 10. ** (float_of_int (14 - pow)) in
  Float.round (factor *. x) /. factor;;

(* Rounding applied to complex numbers *)
let round (z : complex) : complex =
  if z.im = 0.
    then complex_of_float (round_float z.re)
    else
      { re = round_float z.re;
        im = round_float z.im };;