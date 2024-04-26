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
  the first k elements of l in reverse order, then the other n-k in the original order *)
let split_k (l : string list) (k : int) : string list * string list =
  let rec aux a l i =
    match l with
      | h::t ->
        if i < k
          then aux (h::a) t (i+1)
          else (a, l)
      | [] -> (a,[])
  in
  aux [] l 0;;

(* Extracts the last k elements of list l *)
let first_k (l : string list) (k : int) : string list =
  let rec aux l acc i =
    if i >= k then acc
    else
      match l with
        | h::t -> aux t (h::acc) (i+1)
        | [] -> acc
  in
  List.rev (aux l [] 0);;

(* Extracts the last k elements of list l *)
let last_k (l : string list) (k : int) : string list =
  let n = List.length l in
  let rec aux l i =
    if i >= n-k then l
    else
      match l with
        | _::t -> aux t (i+1)
        | [] -> []
  in
  aux l 0;;

(* Returns a list of n copies of the string s *)
let create_sl (n : int) (s : string) : string list =
  let rec aux acc k =
    if k = 0
      then acc
      else aux (s::acc) (k-1)
  in
  aux [] n;;


