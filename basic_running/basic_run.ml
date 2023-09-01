(* Execution of Basic code *)

(* #use "basic_parsing/basic_type.ml"
#use "basic_parsing/basic_encoding.ml" *)
(* #use "basic_running/arithmetic_parsing.ml"
#use "basic_running/graphic.ml" *)

(* Type for the parameter container (defined below) *)
type parameters = {
  proj : project_content;
  var : float array;
  polar : bool;
  xmin : float;
  xmax : float;
  xstep : float;
  ymin : float;
  ymax : float;
  ystep : float;
  axes : bool;
}

(* Executes the Disp (black triangle) operation *)
let disp (writing_index : int ref) : unit =
  if !writing_index = 7
    then (scroll (); decr writing_index);
  clear_line !writing_index;
  print_disp !writing_index;
  tdraw ();
  wait_enter ();
  clear_line !writing_index;
  tdraw ();;

(* Stores the value z in the Ans variable *)
let store_ans (var : float array) (z : complex) : unit =
  var.(28) <- z.re;
  var.(28+29) <- z.im;;

(* General execution function *)
let run (p : project_content) ((code, proglist): basic_code) : unit =
  
  (* Initialization of all parameters *)
  let (param : parameters) = {
    (* proj: Contains the lists, matrices, pictures, captures and strings *)
    proj = p;
    
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
  wipe gscreen;
  writing_index := 0;

  (* Looping function *)
  let rec aux i =
    if i >= n then (wait_enter (); close_graph ())
    else
      match code.(i) with

        | Goto j -> aux j

        | If (e,j) ->
          if is_not_zero (eval param e)
            then aux (i+1)
            else aux j
          
        | Expr (Arithm al) ->
          let z = eval param (Arithm al) in
          (store_ans param.var z;
          if i = n-2
            && (code.(i+1) = End || code.(i+1) = Disp)
            && !prog_goback = []
            then (* End of the program *)
              (if code.(i+1) = Disp then
                (print_number z param.polar;
                disp writing_index);
              print_number z param.polar;
              wait_enter ();
              close_graph ())
            else if i<n-1 && code.(i+1) = Disp
              then
                (print_number z param.polar;
                disp writing_index;
                aux (i+2))
              else aux (i+1))
          
        | Assign (e, v) ->
          let z = eval param e in
          (match v with
            | Var i ->
              (param.var.(i) <- z.re;
              param.var.(i+29) <- z.im;
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