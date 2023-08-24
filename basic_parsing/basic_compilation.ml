(* Compilation of Basic code *)

#use "basic_parsing/basic_type.ml"

(* Data structure:
  We store the code in an array, that doubles its size whenever it is full.
  Amortized complexity: O(1) *)

(* If t is an array of length n, returns a new array of size 2n, with the elements of t copied as
  the first n elements of the new array *)
let double_size (t : basic_code) : basic_code =
  let n = Array.length t in
  let new_t = Array.make (2*n) Empty in
  Array.iteri (fun i x -> new_t.(i) <- x) t;
  new_t;;

(* Sets the cell of index i of the array referenced by t to command x *)
(* When the array is full, the size is doubled and t references the new array *)
let set (t : basic_code ref) (i : int) (comm : command) : unit =
  let n = Array.length !t in
  if i < n
    then !t.(i) <- comm
    else if i = n
      then
        (t := double_size !t;
        !t.(i) <- comm)
      else failwith ("set: Incorrect index i = "^string_of_int i);;

(* Extracts the non-empty part of the array t and returns it *)
let extract (t : basic_code) : basic_code =
  let n = Array.length t in
  (* Searching for last index containing an object *)
  let i = ref (n-1) in
  while !i >= 0 && t.(!i) = Empty do
    decr i
  done;
  if !i = -1
    then failwith "extract: Empty code"
    else Array.init (!i+1) (fun j -> t.(j));;


(********************************************************************************)

(** Compilation **)

(* To be adapted from the lexing function of arithmetic_parsing.ml *)
let rec extract_expr (lexlist : string list) : basic_expr * (string list) =
  match lexlist with
    (* Temporary *)
    | _::"EOL"::t -> ((Expr []), t)
    | _::t -> extract_expr t
    | [] -> failwith "extract_expr: Empty list of lexemes";;

(* Working memory type *)
type working_mem =
  {
    (* ifindex is a pile containing the indices of the last if statements encountered not yet closed *)
    mutable ifindex : int list;
    (* elseindex is a pile containing the lists of indices of else statements
      (several or no ElseIf statements, then one Else statement), each associated with an If statement *)
    mutable elseindex : int list list;
    (* Contains the indices pointed at by the labels *)
    (* Authorized labels are A..Z, r, theta *)
    (* In Casio Basic, when there are several instances of Lbl A, only the first is taken into account. *)
    lblindex : int array;
    
    mutable whileindex : int list;
    mutable forindex : int list;
  };;

(* Function to add an "Else" statement to the first list of the int list list elseindex *)
let add_else (mem : working_mem) (i : int) : unit =
  match mem.elseindex with
    | l::t -> mem.elseindex <- (i::l)::t
    | [] -> mem.elseindex <- [[i]];;

(* Treats the next lexemes in the list of lexemes lexlist, by modifying the array code in place. *)
let treat_commands (code : basic_code ref) (lexlist : string list) : unit =
  let mem =
    {
      ifindex = [];
      elseindex = [];
      lblindex = [||];
      whileindex = [];
      forindex = []
    }
  in

  (* Looping function *)
  
  (* elseindex is a pile containing the index of the last else statements encountered not yet closed *)
  let rec aux (lexlist : string list) (i : int) : unit =
    (* Debugging *)
    if lexlist <> [] then print_endline (List.hd lexlist);
    if mem.ifindex <> [] then (print_int (List.hd mem.ifindex); print_newline ()) else print_endline "empty ifindex";
    match lexlist with

      | "EOL" :: t -> aux t i
      
      (* If, Then, Else, IfEnd *)
      (*  If expr1
          Then prog1
          Else If expr2
          Then prog2
          Else If expr3
          Then prog3
          Else prog4
          IfEnd
          prog5
        ->
          10      If (expr1, 23)
          11..21  prog1
          22      Goto(53)
          23      If (expr2, 30)
          24..28  prog2
          29      Goto(53)
          30      If (expr3, 44)
          31..42  prog3
          43      Goto(53)
          44..52  prog4
          53...   prog5
       *)
      (* New If (not ElseIf, treated below) *)
      | "IF" :: t ->
        let e, t' = extract_expr t in
        (set code i (If (e, -1));
        mem.ifindex <- i::mem.ifindex;
        (* Debugging *)
        print_int (List.hd mem.ifindex);
        print_newline ();
        mem.elseindex <- []::mem.elseindex;
        aux t' (i+1))

      | "THEN" :: t ->
        (try
          if List.hd mem.ifindex = i-1
            then aux t i
            else failwith "Compilation error: Unexpected Then, does not follow an If statement"
        with
          | Failure _ -> failwith "Compilation error: Unexpected Then with no opened If statement")
      
      (* I treat "Else If" statements separately from Else because one IfEnd closes all the potential
        "Else If" and the potential Else following an If *)
      | "ELSE" :: "IF" :: t ->
        let e, t' = extract_expr t in
        (* One cell is left empty to add a Goto when the IfEnd is encountered *)
        (set code (i+1) (If (e, -1));
        (try
          let jif = List.hd mem.ifindex in
          (match (!code).(jif) with
            | If (e,k) ->
              if k = -1
                then
                  set code jif (If (e,i+1))
                else failwith "Compilation error: Unexpected Else If"
            | _ -> failwith "Compilation error: Unexpected Else If")
        with
          | Failure _ -> failwith "Compilation error: Unexpected Else If with no opened If statement");
        mem.ifindex <- (i+1)::mem.ifindex;
        add_else mem (i+1);
        aux t' (i+2))
          
      | "ELSE" :: t ->
        (try
          let jif = List.hd mem.ifindex in
          (match (!code).(jif) with
            | If (e,k) ->
              if k = -1
                (* One cell is left empty to add a Goto when the IfEnd is encountered *)
                then
                  (set code jif (If (e,i));
                  add_else mem i;
                  aux t (i+1))
                (* If was already treated *)
                else failwith "Compilation error: Unexpected Else"
            | _ -> failwith "Compilation error: Unexpected Else")
        with
          | Failure _ -> failwith "Compilation error: Unexpected Else with no opened If statement")

      | "IFEND" :: t ->
        (match mem.elseindex with
          | eil::_ -> List.iter (fun j -> set code j (Goto i)) eil
          | [] ->
            (try
              let jif = List.hd mem.ifindex in
              (match (!code).(jif) with
                | If (e,k) ->
                  if k = -1
                    then
                      (set code jif (If (e,i));
                      aux t i)
                    else failwith "Compilation error: Unexpected IfEnd"
                | _ -> failwith "Compilation error: Unexpected IfEnd")
            with
              | Failure _ -> failwith "Compilation error: Unexpected IfEnd with no opened If statement"))


      (* Errors *)
      | lex :: _ -> failwith ("treat_commands: Unexpected command "^lex)

      (* End case *)
      | [] -> ()
  in

  aux lexlist 0;;



(* Compiles the list of lexemes lexlist into an object of type basic_code *)
let compile (lexlist : string list) : basic_code =
  let code = ref (Array.make 50 Empty) in
  treat_commands code lexlist;
  extract !code;;

(* Buggy, to be fixed *)