(* Compilation of Basic code *)

(* #use "basic_parsing/basic_type.ml"
#use "basic_parsing/project_type.ml"
#use "basic_running/arithmeric_parsing.ml" *)



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
    else
      (t := double_size !t;
      !t.(i) <- comm);;

(* Extracts the non-empty part of the array t and returns it *)
let extract_non_empty (t : command array) : command array =
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

(** Compilation error **)

exception Compilation_error of string list * string;;

(* Raises the exception *)
let fail (t : string list) (error_message : string) =
  raise (Compilation_error (t, error_message));;

(* Skip to the next EOL or DISP *)
let rec extract_line (lexlist : string list) : string list * string list =
  let rec aux (acc : string list) (l : string list) =
    match l with
      | a::t ->
        if a = "EOL" || a = "COLON" || a = "DISP"
          then (a::acc, t)
          else aux (a::acc) t
      | [] -> (acc, [])
  in
  aux [] lexlist;;

(* Prints one line of the program *)
let print_lexline (line : string list) (eol : string) : unit =
  List.iter (fun s -> print_string (String.escaped s); print_char ' ') line;
  print_endline eol;;

let skip_until (lexlist : string list) (i : int) =
  let rec aux prev1 prev2 prev3 prev4 prev5 l k =
    match l with
      | [] -> (prev1, prev2, prev3, prev4, prev5, [])
      | _ ->
        let (line, t') = extract_line l in
        let line_len = List.length line in
        if k + line_len > i
          then (prev1, prev2, prev3, prev4, prev5, l)
          else aux prev2 prev3 prev4 prev5 line t' (k+line_len)
      
  in
  aux [] [] [] [] [] lexlist 0;;


(* Prints 5 lines before index i and 5 lines after *)
let print_around (lexlist : string list) (i : int) : unit =
  let rec print_five (l : string list) (k : int) =
    if k < 5 then
      (let (line, t) = extract_line l in
      match line with
        | eol :: sl ->
          (print_lexline (List.rev sl) eol;
          print_five t (k+1))
        | [] -> ())
  in

  let (prev1, prev2, prev3, prev4, prev5, t) = skip_until lexlist i in
  List.iter
    (fun s ->
      match s with
        | eol::sl -> print_lexline (List.rev sl) eol
        | [] -> ())
    [prev1; prev2; prev3; prev4; prev5];
  print_endline ">>> Error in this line:";
  let (line, t') = extract_line t in
  match line with
    | eol :: sl ->
      (print_lexline (List.rev sl) eol;
      print_endline "<<<";
      print_five t' 0)
    | [] -> ();;


(********************************************************************************)

(** Compilation **)

(* After a double-quote was encountered, extracts the string that follows,
  until another double-quote is encountered *)
(* Returns (sl, t), where sl is the output string, as list of lexemes, in reverse order,
   and t is the tail of lexlist after the second double-quote (excluded) *)
let extract_str (lexlist : string list) : (string list) * (string list) =
  let rec aux sl l =
    match l with
      | s::"QUOTE"::t ->
        if s = "\092" (* anti-slash *)
          then aux ("\034"::sl) t (* The quote is kept *)
          else (s::sl, t)
      | "COLON"::_
      | "EOL"::_ -> fail lexlist "extract_str: string ends without closing \""
      | s::t -> aux (s::sl) t
      | [] -> fail lexlist "extract_str: program ends without closing \""
  in
  aux [] lexlist;;

(* Working memory type *)
type working_mem =
  {
    (* stack: a pile containing the type and index of the last if, while, for or do statements encountered
       that are not yet closed
      Each element is given as (name, i), where name = "if", "else", "while", "for" or "do",
      and i is the index where the statement was encountered *)
    mutable stack : (string * int) list;
    
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
    IfEnd
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
  let (e, t') = extract_num_expr t in
  set code i (If (e, -1));
  mem.stack <- ("if", i)::mem.stack;
  ((i+1), t');;

(* Then *)
let process_then i t code mem =
  try
    if List.hd mem.stack = ("if", i-1)
      then (i,t)
      else fail t "Compilation error: Unexpected Then, does not follow an If statement"
  with
    | Failure _ -> fail t "Compilation error: Unexpected Then with no opened If statement";;

(* Else *)
let process_else i t code mem =
  try
    let (s,jif) = List.hd mem.stack in
    if s <> "if" then
      fail t "Compilation error: Unexpected Else with no opened If statement";
    (match (!code).(jif) with
      | If (e,k) ->
        if k = -1
          (* One cell is left empty to add a Goto when the IfEnd is encountered *)
          then
            (set code jif (If (e,(i+1)));
            mem.stack <- ("else", i)::mem.stack;
            ((i+1),t))
          (* The If was already treated *)
          else fail t "Compilation error: Unexpected Else"
      | _ -> fail t "Compilation error: Unexpected Else")
  with
    | Failure _ -> fail t "Compilation error: Unexpected Else with no opened If statement";;

(* IfEnd *)
let process_ifend i t code mem =
  match mem.stack with
    (* If Then IfEnd (no Else): setting up the If *)
    | ("if", jif)::stack_t ->
      (mem.stack <- List.tl mem.stack;
      match (!code).(jif) with
        | If (e,k) ->
          if k = -1
            then
              (set code jif (If (e,i));
              mem.stack <- stack_t;
              (i,t))
            else fail t "Compilation error: Unexpected IfEnd"
        | _ -> fail t "Compilation error: Unexpected IfEnd")
    (* If Then Else IfEnd*)
    | ("else", jelse)::("if", _)::stack_t ->
      (set code jelse (Goto i);
      mem.stack <- stack_t;
      (i,t))
    | _ -> fail t "Compilation error: Unexpected IfEnd with no opened If statement";;


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
  let (e, t') = extract_num_expr t in
  set code i (If (e, -1));
  mem.stack <- ("while", i)::mem.stack;
  ((i+1), t');;

let process_whileend i t code mem =
  try
    let (s,jwh) = List.hd mem.stack in
    if s <> "while" then
      fail t "Compilation error: Unexpected WhileEnd";
    mem.stack <- List.tl mem.stack;
    set code i (Goto jwh);
    (match (!code).(jwh) with
      | If (e,k) ->
        if k = -1
          then
            (set code jwh (If (e,i+1));
            ((i+1),t))
          else fail t "Compilation error: Unexpected WhileEnd"
      | _ -> fail t "Compilation error: Unexpected WhileEnd")
  with
    | Failure _ -> fail t "Compilation error: Unexpected WhileEnd with no opened While statement";;



(* For To Step Next *)
(* Subtlety:
  - The "To" and "Step" values are evaluated once when the For is encountered
  - If Step is negative, the loop condition is var >= expr2,
    otherwise it is var <= expr2 *)
(*  For expr1 -> var To expr2 Step expr 3
    prog1
    Next
    prog2
  ->
    10      For (var, expr1, expr2, expr3, inext)
    11..21  prog1
    22      Next
    23..    prog2
*)

let process_for i t code mem =
  let (e1, t2) = extract_num_expr t in
  if e1 = QMark
    then fail t "Compilation error: ? cannot set the variable of a For";
  mem.stack <- ("for", i)::mem.stack;
  match t2 with
    | "ASSIGN"::v::"TO"::t3 ->
      (if not (is_var v)
        then fail t "Compilation error: wrong variable in a For loop";
      let (e2, t4) = extract_num_expr t3 in
      if e2 = QMark
        then fail t "Compilation error: ? cannot set the bound of a For";
      match t4 with
        | "STEP"::t5 ->
          (let (e3, t6) = extract_num_expr t5 in
          if e3 = QMark
            then fail t "Compilation error: ? cannot set the bound of a For";
          match t6 with
            | "EOL" :: t7
            | "COLON" :: t7 -> 
              (set code i (For (var_index v, e1, e2, e3, -1));
              (i+1, t7))
            | _ -> fail t "Compilation error: Syntax error after For loop definition"
          )
        | _ -> (* No Step value, 1 by default *)
          (set code i (For (var_index v, e1, e2, Arithm [Entity (Value {re = 1.; im = 0.})], -1));
          (i+1, t4))
      )
        
    | _ -> fail t "Compilation error: Syntax error in a For loop definition";;

let process_next i t code mem =
  try
    let (s,jfor) = List.hd mem.stack in
    if s <> "for" then
      fail t "Compilation error: Unexpected Next";
    mem.stack <- List.tl mem.stack;
    set code i Next;
    (match (!code).(jfor) with
      | For (v,e1,e2,e3,k) ->
        if k = -1
          then
            (set code jfor (For (v,e1,e2,e3,i+1));
            ((i+1),t))
          else fail t "Compilation error: Unexpected Next"
      | _ -> fail t "Compilation error: Unexpected Next")
  with
    | Failure _ -> fail t "Compilation error: Unexpected Next with no opened For statement";;

(* Do, LpWhile *)
let process_lpwhile i t code mem =
  let (e, t') = extract_num_expr t in
  if e = QMark
    then fail t "Compilation error: ? cannot be the condition of a Do-LpWhile";
  match mem.stack with
    | ("do", jdo)::q ->
      (set code i (JumpIf (e, jdo));
      mem.stack <- q;
      (i+1, t'))
    | _ -> fail t "Compilation error: Unexpected LpWhile with no opened Do statement";;

(* Lbl, Goto *)

(* Lbl *)
(* "LBL" is immediately followed by a, then eol in the original list of lexemes *)
let process_lbl i t code mem (a : string) (eol : string) =
  if not (is_var a)
    then fail t "Compilation error: Wrong Lbl index"
    else if eol <> "EOL" && eol <> "COLON"
      then fail t "Compilation error: Syntax error, a Lbl is supposed to be followed by EOL"
      else
        let a_index = var_index a in
        if mem.lblindex.(a_index) <> -1
          then (i,t)
          else
            (mem.lblindex.(a_index) <- i;
            (i,t));;

(* Goto *)
let process_goto i t code mem (a : string) (eol : string) =
  if not (is_var a)
    then fail t "Compilation error: Wrong Goto index"
    else if eol <> "EOL" && eol <> "COLON"
      then fail t "Compilation error: Syntax error, a Goto is supposed to be followed by EOL"
      else
        let a_index = var_index a in
        (if mem.lblindex.(a_index) <> -1
          then set code i (Goto (mem.lblindex.(a_index)))
          else mem.gotoindex <- (a_index,i)::mem.gotoindex;
        ((i+1),t))

(* Prog *)
let process_prog i t code mem =
  let (sl, t') = extract_str t in
  let s = String.concat "" (List.rev sl) in
  match t' with
    | "EOL"::t''
    | "COLON"::t'' ->
      (set code i (Prog s);
      ((i+1),t''))
    | _ -> fail t "Compilation error: Syntax error, a Prog is supposed to be followed by EOL";;

(* Locate *)
let process_locate i t code mem =
  let (e1, t2) = extract_num_expr t in
  try
    if List.hd t2 <> ","
      then fail t "Compilation error: Syntax error in Locate command";
    let (e2, t3) = extract_num_expr (List.tl t2) in
    let t' =
      match t3 with
        | ","::"QUOTE"::t4 ->
          (let (sl, t5) = extract_str t4 in
          set code i (Locate (e1, e2, Loc_text sl));
          t5)
        | ","::t4 ->
          (let (e3, t5) = extract_num_expr t4 in
          set code i (Locate (e1, e2, Loc_expr e3));
          t5)
        | _ -> fail t "Compilation error: Syntax error in Locate command"
      in
    (i+1,t')
  with
    | Failure _ -> fail t "Compilation error: Syntax error in Locate command";;

(* Compiles the list of lexemes lexlist by modifying the array code in place. *)
(* Returns the proglist parameter of the working memory *)
let process_commands (code : (command array) ref) (prog : ((string * (string list)) list)) : (string * int) list =
  let mem =
    {
      stack = [];
      lblindex = Array.make 28 (-1);
      gotoindex = [];
      progindex = [];
    }
  in

  (* Looping function *)
  
  let rec aux (lexlist : string list) (i : int) : int =

    (* print_endline (string_of_int i); *)

    (* Expression handling *)
    let (e, expr_type, t) = extract_expr lexlist in
    let (expr_found, j, next_t) =
      (match e, t with
      
        | Arithm [], _ ->
          (* The beginning of lexlist is not an expression *)
          (false, -1, [])

        (* To be combined with the case below *)
        | _, "ASSIGN"::t' ->
          (match expr_type, t' with
            | Numerical, [v] ->
              (if is_var v then
                (set code i (Assign (e, Var (var_index v)));
                (true, i+1, []))
              else fail lexlist "Compilation: Wrong assignment (->) of a variable")
            | _, v::"EOL"::t''
            | _, v::"COLON"::t''
            | _, v::"DISP"::t'' ->
              if is_var v && expr_type = Numerical then
                (set code i (Assign (e, Var (var_index v)));
                (true, i+1, List.tl t'))
              else
                fail lexlist "Compilation error: Wrong assignment (->) of a variable"

            | Numerical, "LIST"::_::"LSQBRACKET"::_ ->
              let (li,t'') = extract_list_index (List.tl t') in
              (match li with
                | Entity (Variable lx) ->
                  (set code i (Assign (e, lx));
                  (true, i+1, t''))
                | _ -> fail lexlist "Compilation error: Wrong assignment (->) of a list element")

            | Numerical, "MAT"::_::"LSQBRACKET"::_ ->
              let (mi,t'') = extract_mat_index (List.tl t') in
              (match mi with
                | Entity (Variable mx) ->
                  (set code i (Assign (e, mx));
                  (true, i+1, t''))
                | _ -> fail lexlist "Compilation error: Wrong assignment (->) of a matrix element")

            | ListExpr, "LIST"::a::t'' ->
              if is_var a || a = "ANS" then
                (set code i (AssignList (e, Variable (Var (var_index a))));
                (true, i+1, t''))
              else if is_digit a then
                let (vi,q) = read_int (a::t'') true in
                (set code i (AssignList (e, Value (complex_of_int vi)));
                (true, i+1, t''))
              else fail lexlist  "Compilation error: wrong list index"

            | MatExpr, "MAT"::a::t'' ->
              if is_var a && a <> "SMALLR" && a <> "THETA" || a = "ANS" then
                (set code i (AssignMat (e, var_index a));
                (true, i+1, t''))
              else fail lexlist  "extract_mat_index: wrong matrix index"

            (* Errors *)
            | Numerical, "LIST"::_
            | Numerical, "MAT"::_
            | ListExpr, "MAT"::_
            | MatExpr, "LIST"::_
            | ListExpr, _::_
            | MatExpr, _::_ -> fail lexlist "Compilation error: incompatible types during assignment"
            | _ -> fail lexlist "Compilation error: Syntax error during assignment"
          )
        
        (* QMark is treated in the case above *)
        | QMark, _ -> fail lexlist "Compilation error: Syntax error, ? should be followed by ->"
          
        (* e => command *)
        (* Compiled as
          i          If (e, j)
          i+1..j-1   command
          j          ...
        *)
        | _, "IMPL"::t' ->
          let (next_line, t'') = extract_line t' in
          (* Compilation of only the next line *)
          (try
            let j = aux (List.rev next_line) (i+1) in
            set code i (If (e, j-1));
            (true, j-1, t'')
          with
            | Compilation_error (_, error_message) -> fail lexlist error_message)
        
        | _, "EOL"::_
        | _, "COLON"::_
        | _, "DISP"::_
        | _, [] ->
          (set code i (Expr (e, Numerical)); (* Simple expression *) (* To do: implement list_expr and mat_expr *)
          (true, i+1, t))

        | _ -> fail lexlist "Compilation error: Syntax error after an expression"
      )
    in
    if expr_found then aux next_t j
    else

    (* Other lexemes *)
    match lexlist with
      
      | "EOL" :: t
      | "COLON" :: t -> aux t i
      
      | "IF" :: t ->
        let (j,t') = process_if i t code mem in
        aux t' j

      | "THEN" :: t ->
        let (j,t') = process_then i t code mem in
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

      | "FOR" :: t ->
        let (j,t') = process_for i t code mem in
        aux t' j

      | "NEXT" :: t ->
        let (j,t') = process_next i t code mem in
        aux t' j

      | "DO" :: t ->
        (mem.stack <- ("do", i)::mem.stack;
        aux t i)

      | "LPWHILE" :: t ->
        let (j,t') = process_lpwhile i t code mem in
        aux t' j

      (* Strings *)
      | "QUOTE" :: a :: t ->
        if a = "QUOTE"
          then
            (set code i (String []);
            aux t (i+1))
          else
            let (sl, t') = extract_str (a::t) in
            (set code i (String sl);
            aux t' (i+1))

      | "LOCATE" :: t ->
        let (j,t') = process_locate i t code mem in
        aux t' j

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
      
      (* Comment ('): line ignored *)
      | "\039"::t ->
        let (_, t') = extract_line t in
        aux t' i

      | "CLRTEXT" :: eol :: t ->
        (if eol <> "EOL" && eol <> "COLON" && eol <> "DISP" then
          fail (eol :: t) "Compilation error: CLRTEXT should be followed by Eol";
        set code i (Function "CLRTEXT");
        aux t (i+1))

      (* Errors *)
      | lex :: _ -> fail lexlist ("Compilation error: Unexpected command "^(String.escaped lex))

      (* End case *)
      | [] -> i+1 (* Return value *)
  in

  (* Called on all programs *)
  (* Compiles the program and returns the next index *)
  let compile_prog i (name,lexlist) =
    mem.progindex <- (name,i)::mem.progindex;
    for k = 0 to 27 do
      mem.lblindex.(k) <- -1
    done;
    mem.gotoindex <- [];

    try
      (* Compilation of the program *)
      let j = aux lexlist i in

      (* Post-treatment *)

      set code (j-1) End;
      (* The Goto that were encountered before their labels are set. *)
      List.iter (fun (a_j,k) -> set code k (Goto (mem.lblindex.(a_j)))) mem.gotoindex;
      (* The missing IfEnd are closed. *)
      (* Raises a Compilation_error exception if a keyword different from "if" or "else" is encountered *)
      while mem.stack <> [] do
        let _ = process_ifend (j-1) [] code mem in ()
      done;

      (* Next index *)
      j
    with
      | Compilation_error (t, error_message) ->
        (* Handling of compilation error *)
        (let len_t = List.length t in
        let len_lex = List.length lexlist in
        print_endline "---------------------------------------";
        print_endline ("Compilation error in program "^name);
        print_endline error_message;
        print_newline ();
        print_around lexlist (len_lex - len_t);
        print_endline "---------------------------------------";
        failwith "Compilation aborted")
  in

  (* Compilation of all the programs in the project *)
  let _ = List.fold_left compile_prog 0 prog in
  mem.progindex;;

(* Compiles the list of lexemes lexlist into an object of type basic_code *)
let compile (proglist : program list) : basic_code =
  let code = ref (Array.make 50 Empty) in
  let prog_index = process_commands code proglist in
  (extract_non_empty !code, prog_index);;