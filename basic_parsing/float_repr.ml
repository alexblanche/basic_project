(* Floating point numbers representation *)

(* Comments:
  - All numbers are floating numbers, with digits of precision.
  When in the range [-(10^10-1);10^10-1], the number is printed as an int if it has no decimals. *)

(* Cases:
  Ocaml        -> Casio
  12345.       -> 12345
  1234567891.  -> 1234567891
  12345678912. -> 1.234567891e+10
  123456789123. -> 1.234567891e+11
  (max 12 digits in OCaml before scientific form)
  (max 10 in OCaml)
  1e+15        -> 1e+15
  1.23456789123 -> 1.234567891
  1.23456789123e+12 -> 1.234567891e+12
  0.0124       -> 0.0124
  0.00124      -> 1.24e-03
  0.0001       -> 1e-04
  1e-05        -> 1e-05
*)

(* Returns the string s (representing a float) shortened
  to the first 10 digits (regardless of sign, point and e suffix) *)
let count_ten_digits (s : string) : string =
  let n = String.length s in
  (* Extracts the first ten digits before the possible 'e' *)
  let rec aux i c =
    if i = n || c = 10 || s.[i] = 'e'
      then i
      else if s.[i] >= '0' && s.[i] <= '9'
        then aux (i+1) (c+1)
        else aux (i+1) c
  in
  let ns = String.sub s 0 (aux 0 0) in
  (* If the number is in scientific form, add the suffix "e+xx" *)
  if s.[n-4] = 'e'
    then ns^(String.sub s (n-4) 4)
    else if s.[n-5] = 'e'
      then ns^(String.sub s (n-5) 5)
      else ns
;;

(* Converts a string s representing a float in normal form
  greater than 1. (or inferior to -1.) into scientific form *)
let rec sf_ge1 (s : string) : string =
  let n = String.length s in
  let sign_index =
    if s.[0] = '-' then 2 else 1
  in
  let first_digit =
    String.sub s 0 sign_index
  in
  let decimals =
    String.sub s sign_index (min (n-sign_index) 9)
  in
  let esuffix =
    "e+"^(string_of_int (n-sign_index-1))
  in
  first_digit^"."^decimals^esuffix;;

(* Converts a string s representing a float in normal form
  between -1 and 1 into scientific form *)
  (* (0.0124, "0.0124");
  (0.00124, "1.24e-03");
  (0.0001, "1e-04");
  (1e-05, "1e-05") *)
let rec find_first_nz s i =
  if s.[i] >= '1' && s.[i] <= '9'
    then i
    else find_first_nz s (i+1);;

let rec sf_le1 (s : string) : string =
  let n = String.length s in
  let sign =
    if s.[0] = '-'
      then "-"
      else ""
  in
  (* We assume that there is at least one non-zero digit *)
  let index_first_nz = find_first_nz s 0 in
  let first_digit =
    String.sub s index_first_nz 1
  in
  let decimals =
    String.sub s (index_first_nz+1) (min (n-1-index_first_nz) 9)
  in
  let esuffix =
    let pow =
      if s.[0] = '-'
        then index_first_nz-2
        else index_first_nz-1
    in
    "e-"^(if pow > -10 then "0" else "")^(string_of_int pow)
  in
  sign^first_digit^(if decimals = "" then "" else ".")^decimals^esuffix;;


let float_to_casio (n : float) : string =
  let s = string_of_float n in
  if n >= 1e+10 || n <= -.1e+10 then
    if n >= 1e+12 || n <= -.1e+12
      then count_ten_digits s (* Already in sf *)
      else sf_ge1 s (* nf in OCaml, sf in Casio *)
  else if n > -.0.01 && n < 0.01 then
    if n > -.0.0001 && n < 0.0001
      then count_ten_digits s (* Already in sf *)
      else sf_le1 s (* nf in OCaml, sf in Casio *)
  else
    let res = count_ten_digits s in
    let nres = String.length res in
    if res.[nres-1] = '.'
      then String.sub res 0 (nres-1)
      else res
  ;;