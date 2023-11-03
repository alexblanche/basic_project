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

(* The data in the project_content object cannot be used immediately,
  because a real list can be assigned a complex value at runtime.
  We convert all lists and matrices to complex form (by doubling their size). *)

(* Type for the parameter container *)
type parameters = {

  (* Variables: array of size 2*29, storing the content of each variable A..Z, r, theta, Ans
    as real part in the 29 first cells, and imaginary part in the next 29 *)
  var : float array;

  (* Lists: an array of 26 float arrays
    If a list contains n numbers (real or complex), it has length 2*n,
    n floats followed by n zeros, or n real parts followed by their n imaginary parts *)
  list : float array array;

  (* Matrices: an array of 26 float matrices
    If a matrix contains n rows (of real or complex numbers), it has 2*n rows,
    n float rows followed by n zero rows, or n "real" rows followed by n "imaginary" rows *)
  mat : float array array array;

  (* Picture: an array of 20 matrices containing the pictures *)
  (* The first parameter is the number of lines of the pictures:
    - uncompressed pictures: 2048
    - compressed pictures: <= 1024 *)
  pict : (int * (bool array array)) array;

  (* Captures: an array of 20 matrices containing the captures *)
  capt : (bool array array) array;
  
  (* Strings: an array of 20 strings, stored as lists of lexemes in reverse order *)
  str : (string list) array;

  (* List _[0]: contain strings *)
  listzero : (string list) array;

  (* Recursion variables: can only contain real numbers
    0,1:         R Start, R End
    2,3,4,5:     anStart, a0, a1, a2
    6,7,8,9:     bnStart, b0, b1, b2
    10,11,12,13: cnStart, c0, c1, c2 *)
  (* recurvar : float array; *)
  (* Or: add them to var, with new var indices *)


  (* Complex numbers are represented in polar form if true, in carthesian form (a+ib) otherwise *)
  mutable polar : bool;

  (* Parameters of the V-Window *)
  mutable xmin : float;
  mutable xmax : float;
  mutable xstep : float;
  mutable ymin : float;
  mutable ymax : float;
  mutable ystep : float;

  (* Pointers to the bgscreen and the gscreen *)
  mutable bgscreen : bool array array;
  mutable gscreen : bool array array;

  (* Background picture *)
  mutable bgpict : int;
  (* Axes are displayed *)
  mutable axeson : bool;
  (* Style of lines drawn *)
  mutable style : style;

  (* Sgph [0|1|2] [DrawOn | DrawOff], [Scatter | XyLine], List i1, List i2, [Dot | Square | Cross] *)
  mutable sgph : (bool * drawstat_style * int * int * drawstat_mark) array;
}


(* Generation and conversion *)

(* Returns an empty parameters object *)
let empty_param () : parameters =
  {
    (* Variables: array of size 2*29, storing the content of each variable A..Z, r, theta, Ans
      as real part in the 29 first cells, and imaginary part in the next 29 *)
    var = Array.make (2*29) 0.;

    (* Lists 1 to 26 + List Ans *)
    list = Array.make 27 [||];

    (* Mat A to Z + Mat Ans *)
    mat = Array.make 27 [||];

    pict = Array.make 20 (0,[||]);

    capt = Array.make 20 [||];

    str = Array.make 20 [];

    (* List _[0]: contain strings *)
    listzero = Array.make 26 [];
    
    (* Complex numbers are represented in polar form if true, in carthesian form (a+ib) otherwise *)
    polar = false;

    (* Parameters of the V-Window *)
    xmin = 1.;
    xmax = 127.;
    xstep = 0.;
    ymin = 1.;
    ymax = 63.;
    ystep = 0.;

    (* Pointers to the bgscreen and the gscreen *)
    bgscreen = [||];
    gscreen = [||];

    (* Index of the background picture *)
    bgpict = -1;
    (* Axes are displayed *)
    axeson = false;
    (* Style *)
    style = StyleNormal;

    (* DrawStat parameters *)
    sgph =
      [| (false, XYLine, (-1), (-1), DSMDot);
         (false, XYLine, (-1), (-1), DSMDot);
         (false, XYLine, (-1), (-1), DSMDot) |];
  };;

(* Returns a new array with twice the size of t, with the elements of t as first elements
  and a as last elements *) 
let double_array (t : 'a array) (a : unit -> 'a) : 'a array =
  let n = Array.length t in
  Array.init (2*n) (fun i -> if i<n then t.(i) else a ());;

(* Returns a new parameters object, with the lists, matrices, pictures, captures and strings of
  the project_content proj *)
let new_param (proj : project_content) : parameters =
  let p = empty_param () in 

  (* Copy of the lists (converted to complex lists) *)
  Array.iteri
    (fun i (real, t) ->
      if real
        then p.list.(i) <- double_array t (fun () -> 0.)
        else p.list.(i) <- t)
    proj.list;
  
  (* Copy of the matrices (converted to complex matrices) *)
  Array.iteri
    (fun i (real, m) ->
      if m = [||]
        then p.mat.(i) <- [||]
        else
          let col = Array.length m.(0) in
          if real
            then p.mat.(i) <- double_array m (fun () -> Array.make col 0.)
            else p.mat.(i) <- m)
    proj.mat;
  
  (* Copy of the pictures, captures and strings *)
  Array.iteri (fun i nm -> p.pict.(i) <- nm) proj.pict;
  Array.iteri (fun i c -> p.capt.(i) <- c) proj.capt;
  Array.iteri (fun i s -> p.str.(i) <- str_to_rev_symblist_full s) proj.str;
  
  p;;
