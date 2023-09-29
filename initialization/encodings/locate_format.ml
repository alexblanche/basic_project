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
let split_k (l : string list) (k : int) : string list * string list =
  let rec aux a b l i =
    match l with
      | h::t ->
        if i < k
          then aux (h::a) b t (i+1)
          else aux a (h::b) t (i+1)
      | [] -> (a,b)
  in
  aux [] [] l 0;;




