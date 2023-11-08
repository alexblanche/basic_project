(* Variables and indices in Casio Basic *)

(* Authorized variables:
  Complex:
    ABCDEFGHIJKLMNOPQRSTUVWXYZ
    SMALLR THETA
  
  Real:
    AZERO AONE ATWO ANSTART
    BZERO BONE BTWO BNSTART
    CZERO CONE CTWO CNSTART
    DSTART DPITCH DEND
    FSTART FPITCH FEND
    HSTART HPITCH
    RSTART REND
    XMIN XMAX
    YMIN YMAX
    TTHETAMIN TTHETAMAX TTHETAPITCH
    RIGHTXMIN RIGHTXMAX
    RIGHTYMIN RIGHTYMAX
    RIGHTTTHETAMIN RIGHTTTHETAMAX RIGHTTTHETAPTCH

  Positive real:
    XDOT XSCL YSCL XFCT YFCT
    RIGHTXSCL RIGHTXDOT RIGHTYSCL
*)

(** Identification of variables **)

(* Recognizes letters A,B,...,Z, SMALLR, THETA *)
let is_letter_var (s : string) : bool =
  (String.length s = 1 &&
    let c = Char.code s.[0] in
    c >= 65 && c <= 90)
  || s = "SMALLR"
  || s = "THETA";;

(* Recognizes the window variables *)
let is_window_var (s : string) : bool =
  (s.[0] = 'X' || s.[0] = 'Y')
  &&
  (List.mem (String.sub s 1 (String.length s - 1))
    ["MIN"; "MAX"; "SCL"; "FCT"]
  || s = "XDOT")
;;

(* Recognizes real variables:
  AZERO AONE ATWO ANSTART
  BZERO BONE BTWO BNSTART
  CZERO CONE CTWO CNSTART
  DSTART DPITCH DEND
  FSTART FPITCH FEND
  HSTART HPITCH
  RSTART REND
  TTHETAMIN TTHETAMAX TTHETAPITCH
  RIGHTXMIN RIGHTXMAX
  RIGHTYMIN RIGHTYMAX
  RIGHTTTHETAMIN RIGHTTTHETAMAX RIGHTTTHETAPTCH *)
let is_real_var (s : string) : bool =
  let l = String.length s in
  l >= 4
  &&
  let send = String.sub s 1 (l-1) in
  match s.[0] with
    | 'A' | 'B' | 'C' ->
      List.mem send ["ZERO"; "ONE"; "TWO"; "NSTART"]
    | 'D' | 'F' ->
      List.mem send ["START"; "PITCH"; "END"]
    | 'H' ->
      List.mem send ["START"; "PITCH"]
    | 'T' ->
      List.mem send ["THETAMIN"; "THETAMAX"; "THETAPITCH"]
    | _ -> (* 'R' *)
      if s.[1] = 'I'
        then List.mem (String.sub send 4 (l-5)) ["XMIN"; "XMAX"; "YMIN"; "YMAX"; "TTHETAMIN"; "TTHETAMAX"; "TTHETAPTCH"]
        else List.mem send ["START"; "END"]
;;

(* Recognizes positive real variables *)
let is_pos_real_var (s : string) : bool =
  List.mem s ["RIGHTXSCL"; "RIGHTXDOT"; "RIGHTYSCL"]
;;

(* General is_var function *)
let is_var (s : string) : bool =
  is_letter_var s
  || is_window_var s
  || is_real_var s
  || is_pos_real_var s;;


(** Variable indices **)

(* A, B, ..., Z: 0 ... 25
  SMALLR, THETA: 26, 27
  ANS: 28
  AZERO AONE ATWO ANSTART: 29, 30, 31, 32
  BZERO BONE BTWO BNSTART: 33, 34, 35, 36
  CZERO CONE CTWO CNSTART: 37, 38, 39, 40
  DSTART DPITCH DEND: 41, 42, 43
  FSTART FPITCH FEND: 44, 45, 46
  HSTART HPITCH: 47, 48
  RSTART REND: 49, 50
  XMIN XMAX XSCL XFCT XDOT: 51 ... 55
  YMIN YMAX YSCL YFCT: 56 ... 59
  TTHETAMIN TTHETAMAX TTHETAPITCH: 60, 61, 62
  RIGHTXMIN RIGHTXMAX RIGHTXSCL RIGHTXDOT: 63, 64, 65, 66
  RIGHTYMIN RIGHTYMAX RIGHTYSCL: 67, 68, 69
  RIGHTTTHETAMIN RIGHTTTHETAMAX RIGHTTTHETAPTCH: 70, 71, 72
*)

let var_index (s : string) : int =
  let l = String.length s in
  if l = 1 then
    Char.code s.[0] - 65
  else if s = "SMALLR" then 26
  else if s = "THETA" then 27
  else if s = "ANS" then 28
  else
    let send = String.sub s 1 (l-1) in
    match s.[0] with
      | 'A' | 'B' | 'C' ->
        29 + 4*(Char.code s.[0] - 65)
        + List.assoc send [("ZERO", 0); ("ONE", 1); ("TWO", 2); ("NSTART", 3)]
      | 'D' | 'F' ->
        (if s.[0] = 'D' then 41 else 44)
        + List.assoc send [("START", 0); ("PITCH", 1); ("END", 2)]
      | 'H' ->
        if send = "START" then 47 else 48
      | 'X' | 'Y' ->
        (if s.[0] = 'X' then 51 else 55)
        + List.assoc send [("MIN", 0); ("MAX", 1); ("SCL", 2); ("FCT", 3); ("DOT", 4)]
      | 'T' ->
        List.assoc (String.sub send 5 (l-6)) [("MIN", 60); ("MAX", 61); ("PITCH", 62)]
      | _ (* 'R' *) ->
        if s.[1] = 'I' && l >= 9 then
          (* RIGHT... *)
          if s.[5] = 'X' || s.[5] = 'Y' then
            (* RIGHTX... or RIGHTY... *)
            (if s.[5] = 'X' then 63 else 67)
            + List.assoc (String.sub send 5 (l-6)) [("MIN", 0); ("MAX", 1); ("SCL", 2); ("DOT", 3)]
          else (* RIGHTTTHETA... *)
            70 + List.assoc (String.sub send 10 (l-11)) [("MIN", 0); ("MAX", 1); ("PTCH", 2)]
        else
          (* RSTART, REND *)
          if send = "START" then 49 else 50
;;

(* Hard-coded indices for window variables *)
let xmin_index = 51;;
let xmax_index = 52;;
let xscl_index = 53;;
let xdot_index = 55;;
let ymin_index = 56;;
let ymax_index = 57;;
let yscl_index = 58;;


(** Data structure for storage **)
(* Array of floats in this order:
  A,B,...Z, SMALLR, THETA, ANS: real part at index 2*vi, imaginary part at index 2*vi+1
  The others: (real) value at index 58+vi-29 = 29+vi
*)

(* Returns an empty variable array *)
let new_var_storage () : float array =
  Array.make (73 + 29) 0.;;

(* Access to the content of the variable of index vi *)
let access_var (alphamem : float array) (vi : int) : complex =
  if vi <= 28 then
    get_complex alphamem.(2*vi) alphamem.(2*vi+1)
  else complex_of_float alphamem.(29+vi)
;;

(* Returns only the real part of a complex variable, or the content of a real variable *)
let access_real_var (alphamem : float array) (vi : int) : float =
  if vi <= 28
    then alphamem.(2*vi)
    else alphamem.(29+vi)
;;

(* Sets the variable of index vi to the value z *)
let set_var (alphamem : float array) (vi : int) (z : complex) : unit =
  if vi <= 28 then
    (alphamem.(2*vi) <-z.re;
    alphamem.(2*vi+1) <- z.im)
  else
    alphamem.(29+vi) <- z.re
;;

(* Sets only the real part of a complex variable, or the content of a real variable *)
let set_real_var (alphamem : float array) (vi : int) (x : float) : unit =
  if vi <= 28
    then alphamem.(2*vi) <- x
    else alphamem.(29+vi) <- x
;;

