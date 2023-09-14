(* Functions related to complex numbers *)

type complex = Complex.t;;

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
  Float.to_int z.re;;


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
  
(* Returns true if the complex z has a real part close enough to 0.
  and an imaginary part equal to 0.
  It is motivated by the fact that boolean conditions do not involve imaginary parts. *)
let is_zero (z : complex) : bool =
  is_zero_float z.re && z.im = 0.;;
  
let is_not_zero (z : complex) : bool =
  not (is_zero z);;


(** Others **)

(* Returns true if the complex z represents an integer *)
let is_int (z : complex) : bool =
  Float.is_integer z.re && z.im = 0.;;

(** Recoding of the operations **)

(* The exponentiation Complex.pow seems a lot less precise than the float one (**) on float numbers *)
let pow (z1 : complex) (z2 : complex) : complex =
  if z1.im = 0. && z2.im = 0.
    then get_complex (z1.re ** z2.re) 0.
    else Complex.pow z1 z2;;