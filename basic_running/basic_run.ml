(* Execution of Basic code *)

(* #use "basic_parsing/basic_type.ml"
#use "basic_parsing/basic_encoding.ml" *)
(* #use "basic_running/arithmetic_parsing.ml"
#use "basic_running/graphic.ml" *)

let run (p : project_content) ((code, proglist): basic_code) : unit =
  (* Initialization *)
  (* Variables: array of size 2*29, storing the content of each variable A..Z, r, theta, Ans
    as real part in the 29 first cells, and imaginary part in the next 29 *)
  let var = Array.make (2*29) 0. in
  (* prog_goback: pile of indices to return to when the end of a program is reached *)
  let prog_goback = ref [] in
  let n = Array.length code in

  (* Initialization *)
  open_graphic ();
  set_color black;
  clear_text ();
  wipe gscreen;
  wipe gscreen;
  writing_index := 0;

  (* Looping function *)
  let rec aux i =
    if i >= n
      then (disp (); close_graph ())
      else
        match code.(i) with

          | Goto j -> aux j

          | If (e,j) ->
            (* Temporary: adapt eval to basic_expr *)
            if (eval var p (Arithm [Number (Float 0.)])) <> 0.
              then aux (i+1)
              else aux j
          
          | String sl ->
            (if !writing_index = 7
              then (scroll (); decr writing_index);
            clear_line !writing_index;
            locate sl 0 !writing_index;
            incr writing_index;
            tdraw ();
            if i<n-1 && code.(i+1) = Disp
              then
                (if !writing_index = 7
                  then (scroll (); decr writing_index);
                clear_line !writing_index;
                print_disp !writing_index;
                tdraw ();
                disp ();
                clear_line !writing_index;
                tdraw ();
                aux (i+2))
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

          | _ -> failwith ("Runtime error: unexpected command at line "^(string_of_int i))
  in
  
  aux 0;;

(* Important: slow down execution
  An empty for loop executes 798 rounds in 1s,
  for specific functions (mainly display), measurements are needed *)