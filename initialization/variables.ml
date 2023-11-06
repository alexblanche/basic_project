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

(* Returns true if the string s contains exactly one character among A..Z, r, theta *)
  let is_var (s : string) : bool =
    (* (String.length s = 1)
    &&
    (let c = Char.code s.[0] in
    (c >= 65 && c <= 90))
    || s = "SMALLR"
    || s = "THETA";; *)
    let c = Char.code s.[0] in
    if String.length s = 1 then
      c >= 65 && c <= 90
    else
         s = "SMALLR"
      || s = "THETA"
      || (c = 65 && (s = "AZERO" || s = "AONE" || s = "ATWO" || s = "ANSTART"))
      || (c = 66 && (s = "BZERO" || s = "BONE" || s = "BTWO" || s = "BNSTART"))
      || (c = 67 && (s = "CZERO" || s = "CONE" || s = "CTWO" || s = "CNSTART"))
      || (c = 82 && (s = "RSTART" || s = "REND"));;
  
  (* Returns the index of the variables a (A..Z, r, theta, Ans and recursion variables)
    the variable array used in basic_run.ml
    A..Z = 0..25, r = 26, theta = 27, Ans = 28,
    a0,a1,a2,anStart: 29,30,31,32
    b0,b1,b2,bnStart: 33,34,35,36
    c0,c1,c2,cnStart: 37,38,39,40
    R Start, R End: 41, 42 *)
  let var_index (a : string) : int =
    (* match a with
      | "SMALLR" -> 26
      | "THETA" -> 27
      | "ANS" -> 28
      | _ -> (Char.code a.[0]) - 65;; *)
    let c = Char.code a.[0] in
    let l = String.length a in
    if l = 1 then
      c - 65
    else
      match a with
        | "SMALLR" -> 26
        | "THETA" -> 27
        | "ANS" -> 28
        | _ ->
          let k =
            match String.sub a 1 (l-1) with
              | "ZERO" -> 0
              | "ONE" -> 1
              | "TWO" -> 2
              | _ -> 3
          in
          if c = 82 then
            if a = "RSTART"
              then 41
              else 42
          else 29 + 4*(c-65) + k
  ;;