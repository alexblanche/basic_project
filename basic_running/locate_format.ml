(* Conversion functions for Locate display *)

(* For strings that do not contain special characters *)
(* Returns the list of symbols (in string form) of string s in reverse order *)
let str_to_rev_symblist_simple (s : string) : string list =
  String.fold_left (fun acc c -> (String.make 1 c)::acc) [] s;;

(* For strings that contain special characters,
	returns the list of symbols (in string form) in reverse order *)
let str_to_rev_symblist_full (s : string) : string list =
  let n = String.length s in
  let rec aux acc i =
    if i >= n
      then acc
    else if List.mem (Char.code s.[i]) [127;247;249;229;230;231]
      then aux ((String.init 2 (function 0 -> s.[i] | _ -> s.[i+1]))::acc) (i+2)
      else aux ((String.make 1 s.[i])::acc) (i+1)
  in
  aux [] 0;;

(* Splitting when Locate goes out of the screen *)
(* When the list l has length n > k, then the function returns two lists:
  the first k elements of l, then the other n-k *)
let split_k (l : 'a list) (k : int) =
  let rec aux a b l i =
    match l with
      | h::t ->
        if i < k
          then aux (h::a) b t (i+1)
          else aux a (h::b) t (i+1)
      | [] -> (a,b)
  in
  aux [] [] l 0;;


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



