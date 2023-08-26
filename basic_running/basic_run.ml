(* Execution of Basic code *)

#use "basic_parsing/basic_type.ml"
#use "basic_running/arithmetic_parsing.ml"
#use "basic_running/graphic.ml"

let run (code: basic_code) : unit =
  (* Variables: array of size 29, storing the content of each variable A..Z, r, theta, Ans *)
  let var = Array.make 29 0 in
  open_graphic ();
  let n = Array.length code in

  let rec aux i =
    if i >= n
      then (disp (); close_graph ())
      else
        match code.(i) with

          | Goto j -> aux j

          | If (e,j) ->
            (* Temporary: adapt eval to basic_expr *)
            if (eval "0.") = 0.
              then aux j
              else aux (i+1)
          
          | String s ->
            (* Temporary: code the text display *)
            (print_endline s;
            if i<n-1 && code.(i+1) = Disp
              then (disp (); aux (i+2))
              else aux (i+1))
          
                   
          (* to be completed *)
  in
  
  aux 0;;