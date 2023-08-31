(* Execution of Basic code *)

(* #use "basic_parsing/basic_type.ml"
#use "basic_parsing/basic_encoding.ml" *)
(* #use "basic_running/arithmetic_parsing.ml"
#use "basic_running/graphic.ml" *)

let disp (writing_index : int ref) : unit =
  if !writing_index = 7
    then (scroll (); decr writing_index);
  clear_line !writing_index;
  print_disp !writing_index;
  tdraw ();
  wait_enter ();
  clear_line !writing_index;
  tdraw ();;

let store_ans (var : float array) (z : complex) : unit =
  var.(28) <- z.re;
  var.(28+29) <- z.im;;

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
      then (wait_enter (); close_graph ())
      else
        match code.(i) with

          | Goto j -> aux j

          | If (e,j) ->
            if is_not_zero (eval var p e)
              then aux (i+1)
              else aux j
          
          | Expr (Arithm al) ->
            let z = eval var p (Arithm al) in
            (store_ans var z;
            if i<n-1 && code.(i+1) = Disp
              then
                (print_number z false;
                disp writing_index;
                aux (i+2))
              else aux (i+1))
          
          | Assign (e, v) ->
            let z = eval var p e in
            (match v with
              | Var i ->
                (var.(i) <- z.re;
                var.(i+29) <- z.im;
                if i<n-1 && code.(i+1) = Disp
                  then
                    (disp writing_index;
                    aux (i+2))
                  else aux (i+1))
              | _ -> aux (i+1)
              (* | ListIndex (i,e)
              | MatIndex (i,e1,e2)
              | Getkey
              | Random) *) (* to do *)
            )
          
          | String sl ->
            (if !writing_index = 7
              then (scroll (); decr writing_index);
            clear_line !writing_index;
            locate sl 0 !writing_index;
            incr writing_index;
            tdraw ();
            if i<n-1 && code.(i+1) = Disp
              then
                (disp writing_index;
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
                  (wait_enter (); close_graph ()))

          | _ -> failwith ("Runtime error: unexpected command at line "^(string_of_int i))
  in
  
  aux 0;;

(* Important: slow down execution
  An empty for loop executes 798 rounds in 1s,
  for specific functions (mainly display), measurements are needed *)

(* To do:
  When there is an expression at the end of the program, Ans is printed.
  (tests needed to see in which cases exactly) *)