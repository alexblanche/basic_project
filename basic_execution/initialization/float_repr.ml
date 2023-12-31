(* Floating point numbers representation *)

(* The Casio floats are 15 digits long, but are printed in a shorter form.
  OCaml floats are 18 digits long. *)

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
  if n >= 4 && s.[n-4] = 'e'
    then ns^"\015"^(String.sub s (n-3) 3)
    else if n >= 5 && s.[n-5] = 'e'
      then ns^"\015"^(String.sub s (n-4) 4)
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
    let s = String.sub s sign_index (min (n-sign_index) 9) in
    (* Removal of the '0' at the end of the string *)
    let len_s = String.length s in
    if s.[len_s-1] = '0' then
      (let i = ref (len_s - 2) in
      while !i >= 0 && s.[!i] = '0' do
        decr i
      done;
      String.sub s 0 (!i+1))
    else s
  in
  let esuffix =
    "\015+"^(string_of_int (n-sign_index-1))
  in
  first_digit
    ^(if String.exists (fun c -> c<>'0') decimals then ("."^decimals) else "")
    ^esuffix;;

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
    let s = String.sub s (index_first_nz+1) (min (n-1-index_first_nz) 9) in
    (* Removal of the '0' at the end of the string *)
    let len_s = String.length s in
    if len_s >= 1 && s.[len_s-1] = '0' then
      (let i = ref (len_s - 2) in
      while !i >= 0 && s.[!i] = '0' do
        decr i
      done;
      String.sub s 0 (!i+1))
    else s
  in
  let esuffix =
    let pow =
      if s.[0] = '-'
        then index_first_nz-2
        else index_first_nz-1
    in
    "\015-"^(if pow > -10 then "0" else "")^(string_of_int pow)
  in
  sign^first_digit
    ^(if String.exists (fun c -> c<>'0') decimals then "." else "")
    ^decimals^esuffix;;

(* Returns the representation of the OCaml float n as a Casio number *)
let float_to_casio (n : float) : string =
  let s = string_of_float n in
  let ns = String.length s in
  let res =
    (* if (n >= 1e+10 || n <= -.1e+10) && not (n >= 1e+12 || n <= -.1e+12) then
      sf_ge1 s (* nf in OCaml, sf in Casio *)
    else if (n > -.0.01 && n < 0.01) && not (n > -.0.0001 && n < 0.0001) then
      sf_le1 s (* nf in OCaml, sf in Casio *)
    else
      count_ten_digits s (* Already in sf *) *)
    
    (* More robust implementation *)
    if s = "0." || ns >= 4 && s.[ns-4] = 'e' || ns >= 5 && s.[ns-5] = 'e' then
      (* Already in sf *)
      count_ten_digits s
    else
      (* nf in OCaml *)

      if n >= 1e+10 || n <= -.1e+10
        (* Conversion to sf *)
        then sf_ge1 s
      else if n > -.0.01 && n < 0.01
        then sf_le1 s
      else
        (* String remains in nf *)
        count_ten_digits s
  in
  let nres = String.length res in
  if res.[nres-1] = '.'
    then String.sub res 0 (nres-1)
    else
      (* Deletion of zeroes at the end of res, or before the 'e' *)
      (let last_dec =
        if nres >= 4 && res.[nres-4] = '\015'
          then nres-5
          else nres-1
      in
      let i = ref last_dec in
      (* No need to check !i>=0, as the case of n = 0. has already been treated *)
      while res.[!i] = '0' do
        decr i
      done;
      let index_sub =
        if res.[!i] = '.'
          then !i
          else !i+1
      in
      if last_dec = nres-5 then
        (String.sub res 0 index_sub)^(String.sub res (nres-4) 4)
      else
        match String.rindex_from_opt res (min index_sub (nres-2)) '.' with
          | Some _ -> String.sub res 0 index_sub
          | _ -> res
      );;


(** Formatting complex numbers **)

(* Returns the representation of the complex z as a Casio complex number
  under the form a+ib, as a list of strings: [(repr of a); [plus (\137)]; (repr of b); [i (\127\080)]] *)
  let complex_to_casio_aib (z : complex) : string list list =
    [str_to_rev_symblist_simple (float_to_casio z.re);
    ["\137"];
    str_to_rev_symblist_simple (float_to_casio z.im);
    ["\127\080"]];;
  
  (* Same with polar representation of a complex number:
    [repr of r; repr of complex angle (\127\084); repr of theta] *)
  let complex_to_casio_polar (z : complex) : string list list =
    [str_to_rev_symblist_simple (float_to_casio (Complex.norm z));
    ["\127\084"];
    str_to_rev_symblist_simple (float_to_casio (Complex.arg z))];;
  
  
