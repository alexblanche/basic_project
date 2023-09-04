(* Execution of Basic code *)

(* #use "basic_parsing/basic_type.ml"
#use "basic_parsing/basic_encoding.ml" *)
(* #use "basic_running/arithmetic_parsing.ml"
#use "basic_running/graphic.ml" *)

(* Executes the Disp (black triangle) operation *)
let disp (writing_index : int ref) : unit =
  if !writing_index = 7
    then (scroll (); decr writing_index);
  clear_line !writing_index;
  print_disp !writing_index;
  tdraw ();
  wait_enter ();
  clear_line !writing_index;
  tdraw ();; (* Can the last tdraw () be removed to speed up the execution? *)

(* Stores the value z in the Ans variable *)
let store_ans (var : float array) (z : complex) : unit =
  var.(28) <- z.re;
  var.(28+29) <- z.im;;

(* Wait for Enter key to be pressed, then closes the graphic window *)
let quit () : unit =
  wait_enter ();
  close_graph ();;

(* Prints the value of Ans, then quits *)
let quit_ans (ans : complex) (polar : bool) : unit =
  print_number ans polar;
  tdraw ();
  quit ();;


(* General execution function *)
let run (proj : project_content) ((code, proglist): basic_code) : unit =
  
  (* Initialization of all parameters *)
  let (p : parameters) = {
    (* proj: Contains the lists, matrices, pictures, captures and strings *)
    proj = proj;
    
    (* Variables: array of size 2*29, storing the content of each variable A..Z, r, theta, Ans
      as real part in the 29 first cells, and imaginary part in the next 29 *)
    var = Array.make (2*29) 0.;
    
    (* Complex numbers are represented in polar form if true, in carthesian form (a+ib) otherwise *)
    polar = false;

    (* Parameters of the V-Window *)
    xmin = 1.;
    xmax = 127.;
    xstep = 0.;
    ymin = 1.;
    ymax = 63.;
    ystep = 0.;
    (* Display the axes if true *)
    axes = false;

  } in
  
  (* prog_goback: pile of indices to return to when the end of a program is reached *)
  let prog_goback = ref [] in
  let n = Array.length code in

  (* Initialization *)
  open_graphic ();
  set_color black;
  clear_text ();
  wipe gscreen;
  writing_index := 0;

  (* Looping function *)
  let rec aux (i : int) : unit =
    if i >= n then (* End of the execution *)
      quit_ans (get_var_val p.var 28) p.polar
    else

    match code.(i) with

      | Goto j -> aux j

      | If (e,j) ->
        if is_not_zero (eval p e)
          then aux (i+1)
          else aux j
          
      | Expr (Arithm al) ->
        let z = eval p (Arithm al) in
        (store_ans p.var z;
        if i<n-1 && code.(i+1) = Disp
          then
            (print_number z p.polar;
            disp writing_index;
            aux (i+2))
          else aux (i+1))
          
      | Assign (e, v) ->
        let z = eval p e in
        (match v with
          | Var i ->
            (p.var.(i) <- z.re;
            p.var.(i+29) <- z.im;
            if i<n-1 && code.(i+1) = Disp
              then
                (print_number z p.polar;
                disp writing_index;
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
        if (i = n-2 && code.(i+1) = End
          || i = n-3 && code.(i+1) = Disp && code.(i+2) = End)
          && !prog_goback = []
          then (* End of the program *)
            (if code.(i+1) = Disp
              then disp writing_index;
            quit () (* Quit after the string *))
          else if i<n-1 && code.(i+1) = Disp
            then
              (disp writing_index;
              aux (i+2))
            else aux (i+1))
          
      | Locate (e1, e2, sl) ->
        let z1 = eval p e1 in
        let z2 = eval p e2 in
        (if not
          ((is_int z1)
          && (is_int z2)
          && (z1.re >= 1.)
          && (z1.re <= 21.)
          && (z2.re >= 1.)
          && (z2.re <= 7.))
          then failwith "Runtime error: wrong arguments for Locate";
        locate sl (int_of_complex z1) (int_of_complex z2);
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
          | [] -> quit_ans (get_var_val p.var 28) p.polar)

      | _ -> failwith ("Runtime error: unexpected command at line "^(string_of_int i))
  in
  
  aux 0;;

(* Important: slow down execution
  An empty for loop executes 798 rounds in 1s,
  for specific functions (mainly display), measurements are needed *)