(* Compilation of Basic code *)

#use "basic_parsing/basic_type.ml"
#use "basic_parsing/project_type.ml"
#use "basic_running/arithmeric_parsing.ml"

(* Data structure:
  We store the code in an array, that doubles its size whenever it is full.
  Amortized complexity: O(1) *)

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
    else if i = n
      then
        (t := double_size !t;
        !t.(i) <- comm)
      else failwith ("set: Incorrect index i = "^string_of_int i);;

(* Extracts the non-empty part of the array t and returns it *)
let extract (t : command array) : command array =
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

(* After a double-quote was encountered, extracts the string that follows,
  until another double-quote is encountered *)
(* Returns (sl, t), where sl is the output string, as list of lexemes,
   and t is the tail of lexlist after the second double-quote (excluded) *)
let extract_str (lexlist : string list) : (string list) * (string list) =
  let rec aux sl l =
    match l with
      | s::"QUOTE"::t -> (List.rev (s::sl), t)
      | "EOL"::_ -> failwith "extract_str: string ends without closing \""
      | s::t -> aux (s::sl) t
      | [] -> failwith "extract_str: program ends without closing \""
  in
  aux [] lexlist;;

(* Working memory type *)
type working_mem =
  {
    (* stack: a pile containing the type and index of the last if, while, for or do statements encountered
       that are not yet closed
      Each element is given as (name, i), where name = "if", "while", "for" or "do",
      and i is the index where the statement was encountered *)
    mutable stack : (string * int) list;

    (* elseindex: a pile containing the lists of indices of else statements
      (several or no ElseIf statements, then one or no Else statement), each associated with an If statement *)
      (* If a list is empty, then the If was of the form If Then IfEnd *)
    mutable elseindex : int list list;
    
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

(* Function to add an "Else" statement to the first list of the int list list elseindex *)
let add_else (mem : working_mem) (i : int) : unit =
  match mem.elseindex with
    | l::t -> mem.elseindex <- (i::l)::t
    | [] -> mem.elseindex <- [[i]];;

(********************************************************************************)

(** Processing function for each keyword **)
(* Each function has type
  (i : int) (t : string list) (code : string list) (mem : working_mem) : (int * (string list))
  i is the reading position, t is the tail of the list of lexemes before processing,
  code is the current list of lexemes, mem is the working memory.
  Returns the new reading position and the tail of the list of lexemes. *)

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

(* If *)
let process_if i t code mem = 
  let e, t' = extract_expr t in
  set code i (If (e, -1));
  mem.stack <- ("if", i)::mem.stack;
  mem.elseindex <- []::mem.elseindex;
  ((i+1), t');;

(* Then *)
let process_then i t code mem =
  try
    if List.hd mem.stack = ("if", i-1)
      then (i,t)
      else failwith "Compilation error: Unexpected Then, does not follow an If statement"
  with
    | Failure _ -> failwith "Compilation error: Unexpected Then with no opened If statement";;

(* Else If *)
let process_else_if i t code mem =
  let e, t' = extract_expr t in
  (* One cell is left empty to add a Goto when the IfEnd is encountered *)
  set code (i+1) (If (e, -1));
  (try
    let (s,jif) = List.hd mem.stack in
    if s <> "if" then
      failwith "Compilation error: Unexpected Else If with no opened If statement";
    (match (!code).(jif) with
      | If (e,k) ->
        if k = -1
          then set code jif (If (e,i+1))
          else failwith "Compilation error: Unexpected Else If"
      | _ -> failwith "Compilation error: Unexpected Else If")
  with
    | Failure _ -> failwith "Compilation error: Unexpected Else If with no opened If statement");
  mem.stack <- ("if",(i+1))::(List.tl mem.stack);
  add_else mem (i+1);
  ((i+2),t');;

(* Else *)
let process_else i t code mem =
  try
    let (s,jif) = List.hd mem.stack in
    if s <> "if" then
      failwith "Compilation error: Unexpected Else with no opened If statement";
    (match (!code).(jif) with
      | If (e,k) ->
        if k = -1
          (* One cell is left empty to add a Goto when the IfEnd is encountered *)
          then
            (set code jif (If (e,(i+1)));
            add_else mem i;
            ((i+1),t))
          (* The If was already treated *)
          else failwith "Compilation error: Unexpected Else"
      | _ -> failwith "Compilation error: Unexpected Else")
  with
    | Failure _ -> failwith "Compilation error: Unexpected Else with no opened If statement";;

(* IfEnd *)
let process_ifend i t code mem =
  match mem.elseindex with
  (* Adding the Goto before each Else/Else If *)
    | eil::eit ->
      (mem.elseindex <- eit;
      List.iter (fun j -> set code j (Goto i)) eil;
      (i,t))
      (* If Then IfEnd (no Else): setting up the If *)
    | [] ->
      (try
        let (s,jif) = List.hd mem.stack in
        if s <> "if" then
          failwith "Compilation error: Unexpected IfEnd";
        mem.stack <- List.tl mem.stack;
        (match (!code).(jif) with
          | If (e,k) ->
            if k = -1
              then
                (set code jif (If (e,i));
                (i,t))
              else failwith "Compilation error: Unexpected IfEnd"
          | _ -> failwith "Compilation error: Unexpected IfEnd")
      with
        | Failure _ -> failwith "Compilation error: Unexpected IfEnd with no opened If statement");;

(* While, WhileEnd *)
(*  While expr
    prog1
    WhileEnd
    prog2
  ->
    10      If (expr, 23)
    11..21  prog1
    22      Goto(10)
    23..    prog2
*)
let process_while i t code mem =
  let e, t' = extract_expr t in
  set code i (If (e, -1));
  mem.stack <- ("while", i)::mem.stack;
  ((i+1), t');;

let process_whileend i t code mem =
  try
    let (s,jwh) = List.hd mem.stack in
    if s <> "while" then
      failwith "Compilation error: Unexpected WhileEnd";
    mem.stack <- List.tl mem.stack;
    set code i (Goto jwh);
    (match (!code).(jwh) with
      | If (e,k) ->
        if k = -1
          then
            (set code jwh (If (e,i));
            ((i+1),t))
          else failwith "Compilation error: Unexpected WhileEnd"
      | _ -> failwith "Compilation error: Unexpected WhileEnd")
  with
    | Failure _ -> failwith "Compilation error: Unexpected WhileEnd with no opened While statement";;

(* Lbl, Goto *)

(* Lbl *)
(* "LBL" is immediately followed by a, then eol in the original list of lexemes *)
let process_lbl i t code mem (a : string) (eol : string) =
  if not (is_var a)
    then failwith "Compilation error: Wrong label"
    else if eol <> "EOL"
      then failwith "Compilation error: Syntax error, a Lbl is supposed to be followed by EOL"
      else
        (* This absolutely needs to be merged with the "Goto" case below *)
        let c = Char.code a.[0] in
        let a_index =
          if a = "SMALLR" then 26
          else if a = "THETA" then 27
          else c - 65
        in
        if mem.lblindex.(a_index) <> -1
          then (i,t)
          else
            (mem.lblindex.(a_index) <- i;
            (i,t));;

(* Goto *)
let process_goto i t code mem (a : string) (eol : string) =
  if not (is_var a)
    then failwith "Compilation error: Wrong goto"
    else if eol <> "EOL"
      then failwith "Compilation error: Syntax error, a Goto is supposed to be followed by EOL"
      else
        let c = Char.code a.[0] in
        let a_index =
          if a = "SMALLR" then 26
          else if a = "THETA" then 27
          else c - 65
        in
        (if mem.lblindex.(a_index) <> -1
          then set code i (Goto (mem.lblindex.(a_index)))
          else mem.gotoindex <- (a_index,i)::mem.gotoindex;
        ((i+1),t))

(* Prog *)
let process_prog i t code mem =
  let sl, t' = extract_str t in
  let s = String.concat "" sl in
  match t' with
    | "EOL"::t'' ->
      (set code i (Prog s);
      ((i+1),t''))
    | _ -> failwith "Compilation error: Syntax error, a Prog is supposed to be followed by EOL";;

(* Compiles the list of lexemes lexlist by modifying the array code in place. *)
(* Returns the proglist parameter of the working memory *)
let process_commands (code : (command array) ref) (prog : ((string * (string list)) list)) : (string * int) list =
  let mem =
    {
      stack = [];
      elseindex = [];
      lblindex = Array.make 28 (-1);
      gotoindex = [];
      progindex = []
    }
  in

  (* Looping function *)
  
  (* elseindex is a pile containing the index of the last else statements encountered not yet closed *)
  let rec aux (lexlist : string list) (i : int) : int =
    match lexlist with
      
      | "EOL" :: t -> aux t i
      
      | "IF" :: t ->
        let (j,t') = process_if i t code mem in
        aux t' j

      | "THEN" :: t ->
        let (j,t') = process_then i t code mem in
        aux t' j
      
      | "ELSE" :: "IF" :: t ->
        let (j,t') = process_else_if i t code mem in
        aux t' j
          
      | "ELSE" :: t ->
        let (j,t') = process_else i t code mem in
        aux t' j

      | "IFEND" :: t ->
        let (j,t') = process_ifend i t code mem in
        aux t' j
      
      | "WHILE" :: t ->
        let (j,t') = process_while i t code mem in
        aux t' j
      
      | "WHILEEND" :: t ->
        let (j,t') = process_whileend i t code mem in
        aux t' j

      (* Strings *)
      | "QUOTE" :: t ->
        let sl, t' = extract_str t in
        (set code i (String sl);
        aux t' (i+1))

      | "DISP" :: t ->
        (set code i Disp;
        aux t (i+1))
      
      | "LBL"::a::eol::t ->
        let (j,t') = process_lbl i t code mem a eol in
        aux t' j
      
      | "GOTO"::a::eol::t ->
        let (j,t') = process_goto i t code mem a eol in
        aux t' j
      
      | "PROG"::"QUOTE"::t ->
        let (j,t') = process_prog i t code mem in
        aux t' j

      (* Errors *)
      | lex :: _ -> failwith ("Compilation error: Unexpected command "^lex)

      (* End case *)
      (* The Goto that were encountered before their labels are set. *)
      | [] ->
        (List.iter (fun (a_j,i) -> set code i (Goto (mem.lblindex.(a_j)))) mem.gotoindex;
        set code i End;
        (i+1) (* Return value *))
  in

  let _ = List.fold_left
    (fun j (name,lexlist) ->
      (mem.progindex <- (name,j)::mem.progindex;
      aux lexlist j))
      0 prog
  in
  mem.progindex;;



(* Compiles the list of lexemes lexlist into an object of type basic_code *)
let compile (proglist : program list) : basic_code =
  let code = ref (Array.make 50 Empty) in
  let prog_index = process_commands code proglist in
  (extract !code, prog_index);;