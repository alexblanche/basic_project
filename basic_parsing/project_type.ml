(* Type for project content *)

(* Content of a project (g1m file) *)
(* Each object (except programs) is stored at index index_Casio-1. *)

(* The name of the program is stored with the list of lexemes *)
type program = string * (string list)

type project_content =
  {
    prog : program list;

    (* List indices range from 1 to 26 *)
    (* The first parameter is true if the list contains real numbers,
      false if it contains complex numbers *)
    list : (bool * (float array)) array;

    (* Matrix indices range from A to Z *)
    (* The first parameter is true if the matrix contains real numbers,
      false if it contains complex numbers *)
    mat : (bool * (float array array)) array;

    (* Picture indices range from 1 to 20 *)
    (* The first parameter is the number of lines of the pictures:
      - uncompressed pictures: 2048
      - compressed pictures: <= 1024 *)
    pict : (int * (bool array array)) array;

    (* Capture indices range from 1 to 20 *)
    capt : (bool array array) array;

    (* String indices range from 1 to 20 *)
    str : string array
  }


(* Computing the number of elements of a project *)

(* Returns the number of elements of array t that satisfy predicate p *)
let array_count (p : 'a -> bool) (t : 'a array) : int =
  Array.fold_left (fun cpt a -> if p a then (cpt + 1) else cpt) 0 t;;

let non_empty_str (t : string array) : int =
  array_count (fun s -> s <> "") t;;
  
let non_empty_array (t : ('a array) array) : int =
  array_count (fun x -> x <> [||]) t;;
  
let non_empty_pair_int (t : (int * 'a) array) : int =
  array_count (fun (i,_) -> i <> 0) t;;
  
let non_empty_pair_array (t : ('a * ('b array)) array) : int =
  array_count (fun (_,x) -> x <> [||]) t;;
  
let number_of_elements (p : project_content) : int =
  (List.length p.prog)
  + (non_empty_pair_int p.pict)
  + (non_empty_array p.capt)
  + (non_empty_pair_array p.list)
  + (non_empty_pair_array p.mat)
  + (non_empty_str p.str);;


(** Parameters used at runtime **)

(* Type for the parameter container *)
type parameters = {
  (* proj: Contains the lists, matrices, pictures, captures and strings *)
  proj : project_content;
  (* Variables: array of size 2*29, storing the content of each variable A..Z, r, theta, Ans
    as real part in the 29 first cells, and imaginary part in the next 29 *)
  var : float array;
  (* Complex numbers are represented in polar form if true, in carthesian form (a+ib) otherwise *)
  polar : bool;
  (* Parameters of the V-Window *)
  xmin : float;
  xmax : float;
  xstep : float;
  ymin : float;
  ymax : float;
  ystep : float;
  (* Display the axes if true *)
  axes : bool;
}