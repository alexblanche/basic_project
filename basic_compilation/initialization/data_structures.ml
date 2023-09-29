(* Data structures used for compilation:
  resizable array used to store the compiled code
  and working memory type used in the compilation process *)


(** Data structure used to store the compiled code
  We store the code in an array, that doubles its size whenever it is full.
  Amortized complexity of addition of an element: O(1) **)

(* If t is an array of length n, returns a new array of size 2n, with the elements of t copied as
  the first n elements of the new array *)
let double_size (t : (command array)) : command array =
  let n = Array.length t in
  let new_t = Array.make (2*n) Empty in
  Array.iteri (fun i x -> new_t.(i) <- x) t;
  new_t;;

(* Sets the cell of index i of the array referenced by t to command x *)
(* When the array is full, the size is doubled and t references the new array *)
let set (t : (command array) ref) (i : int) (comm : command) : unit =
  let n = Array.length !t in
  if i < n
    then !t.(i) <- comm
    else
      (t := double_size !t;
      !t.(i) <- comm);;

(* Extracts the non-empty part of the array t and returns it *)
let extract_non_empty (t : command array) : command array =
  let n = Array.length t in
  (* Searching for last index containing an object *)
  let i = ref (n-1) in
  while !i >= 0 && t.(!i) = Empty do
    decr i
  done;
  if !i = -1
    then failwith "extract: Empty code"
    else Array.init (!i+1) (fun j -> t.(j));;


(** Working memory type **)
type working_mem =
  {
    (* stack: a pile containing the type and index of the last if, while, for or do statements encountered
       that are not yet closed
      Each element is given as (name, i), where name = "if", "else", "while", "for" or "do",
      and i is the index where the statement was encountered *)
    mutable stack : (string * int) list;
    
    (* Contains the indices pointed at by the labels *)
    (* Authorized labels are A..Z, r, theta *)
    (* In Casio Basic, when there are several instances of Lbl A, only the first is taken into account. *)
    lblindex : int array;

    (* gotoindex: a pile containing, for each Goto encountered, a pair (a,i), where a is the index of
      the Lbl the Goto points to, and i is the index the Goto was encountered *)
    mutable gotoindex : (int * int) list;

    (* progindex: a list containing the name and index in the code of each program in the project *)
    mutable progindex : (string * int) list;
  };;