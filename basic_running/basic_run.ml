(* Execution of Basic code *)

#use "basic_parsing/basic_type.ml"
#use "basic_running/arithmetic_parsing.ml"
#use "basic_running/graphic.ml"

let run ((code, proglist): basic_code) : unit =
  (* Variables: array of size 29, storing the content of each variable A..Z, r, theta, Ans *)
  let var = Array.make 29 0 in
  (* prog_goback: pile of indices to return to when the end of a program is reached *)
  let prog_goback = ref [] in

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
          
          | Prog name ->
            (prog_goback := (i+1)::!prog_goback;
            let j = List.assoc name proglist in
            aux j)

          | End ->
            (match !prog_goback with
              | i::t ->
                  (prog_goback := t;
                  aux i)
              | [] ->
                  (disp (); close_graph ()))

          
  in
  
  aux 0;;