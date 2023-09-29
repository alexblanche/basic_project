(* Auxiliary functions for basic_compilation *)

(* After a double-quote was encountered, extracts the string that follows,
  until another double-quote is encountered *)
(* Returns (sl, t), where sl is the output string, as list of lexemes, in reverse order,
   and t is the tail of lexlist after the second double-quote (excluded) *)
let extract_str (lexlist : string list) : (string list) * (string list) =
  let rec aux sl l =
    match l with
      | "QUOTE"::t -> (sl, t)
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
  (* We skip the "break" statements in mem.stack to reach the "if",
    then we remove the "if" and add back the "break" in the same order,
    and finally add the "else" *)
  let rec aux ms br_acc =
    match ms with
      | ("if", jif)::stack_t ->
        (match (!code).(jif) with
          | If (e,-1) ->
            (* One cell is left empty to add a Goto when the IfEnd is encountered *)
            (set code jif (If (e,(i+1)));
            mem.stack <- ("else", i)::(List.rev_append br_acc stack_t);
            ((i+1),t))
          | _ -> fail t "Compilation error: Unexpected Else")
      | ("break", j)::stack_t ->
        aux stack_t (("break", j)::br_acc)
      | _ -> fail t "Compilation error: Unexpected Else with no opened If statement"
  in
  aux mem.stack [];;

(* IfEnd *)
let process_ifend i t code mem =
  let rec aux ms br_acc =
    match ms with
      (* If Then IfEnd (no Else): setting up the If *)
      | ("if", jif)::stack_t ->
        (match (!code).(jif) with
          | If (e,-1) ->
            (set code jif (If (e,i));
            mem.stack <- List.rev_append br_acc stack_t;
            (i,t))
          | _ -> fail t "Compilation error: Unexpected IfEnd")
      (* If Then Else IfEnd*)
      (* The Goto index of the If was set by the call to process_else
        that put the ("else", jelse) in mem.stack *)
      | ("else", jelse)::stack_t ->
        (set code jelse (Goto i);
        mem.stack <- List.rev_append br_acc stack_t;
        (i,t))
      | ("break", j)::stack_t ->
        aux stack_t (("break", j)::br_acc)
      | _ -> fail t "Compilation error: Unexpected IfEnd with no opened If statement"
  in
  aux mem.stack [];;

(** Loops **)
(* Loop bodies can contain "Break" statements, which we compile as Goto commands pointing
  to the index of the closing statement of the loop. *)

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

let rec process_whileend i t code mem =
  match mem.stack with
    | ("while", jwh)::stack_t ->
      (set code i (Goto jwh);
      mem.stack <- stack_t;
      match (!code).(jwh) with
        | If (e,-1) ->
          (set code jwh (If (e,i+1));
          ((i+1),t))
        | _ -> fail t "Compilation error: Unexpected WhileEnd")
    | ("break", jbr)::stack_t ->
      (set code jbr (Goto (i+1));
      mem.stack <- stack_t;
      process_whileend i t code mem)
    | _ -> fail t "Compilation error: Unexpected WhileEnd";;


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
          (set code i (For (var_index v, e1, e2, Complex {re = 1.; im = 0.}, -1));
          (i+1, t4))
      )
        
    | _ -> fail t "Compilation error: Syntax error in a For loop definition";;

let rec process_next i t code mem =
  match mem.stack with
    | ("for", jfor)::stack_t ->
      (mem.stack <- stack_t;
      set code i Next;
      match (!code).(jfor) with
        | For (v,e1,e2,e3,-1) ->
          (set code jfor (For (v,e1,e2,e3,i+1));
          ((i+1),t))
        | _ -> fail t "Compilation error: Unexpected Next")
    | ("break", jbr)::stack_t ->
      (set code jbr (Goto (i+1));
      mem.stack <- stack_t;
      process_next i t code mem)
    | _ -> fail t "Compilation error: Unexpected Next";;

(* Do, LpWhile *)
let process_lpwhile i t code mem =
  let rec aux ms e t' =
    match ms with
      | ("do", jdo)::stack_t ->
        (set code i (JumpIf (e, jdo));
        mem.stack <- stack_t;
        (i+1, t'))
      | ("break", jbr)::stack_t ->
        (set code jbr (Goto (i+1));
        aux stack_t e t')
      | _ -> fail t "Compilation error: Unexpected LpWhile with no opened Do statement"
  in
  match extract_num_expr t with
    | (QMark, _) -> fail t "Compilation error: ? cannot be the condition of a Do-LpWhile"
    | (e, t') -> aux mem.stack e t';;

(* Lbl, Goto *)

(* Lbl *)
(* "LBL" is immediately followed by a, then eol in the original list of lexemes *)
let process_lbl i t code mem =
  match t with
    | a::t' ->
      if not (is_var a)
        then fail t "Compilation error: Wrong Lbl index"
      else
        let t'' =
          match t' with
            | eol::q ->
              if eol <> "EOL" && eol <> "COLON"
                then fail t "Compilation error: Syntax error, a Lbl is supposed to be followed by EOL"
                else q
            | [] -> []
        in
        let a_index = var_index a in
        if mem.lblindex.(a_index) <> -1
          then (i,t'')
          else
            (mem.lblindex.(a_index) <- i;
            (i,t''))
    | _ -> fail t "Compilation error: Missing an Lbl index";;

(* Goto *)
let process_goto i t code mem =
  match t with
    | a::t' ->
      if not (is_var a)
        then fail t "Compilation error: Wrong Goto index"
      else
        let t'' =
          match t' with
            | eol::q ->
              if eol <> "EOL" && eol <> "COLON"
                then fail t "Compilation error: Syntax error, a Goto is supposed to be followed by EOL"
                else q
            | [] -> []
        in
        let a_index = var_index a in
        (if mem.lblindex.(a_index) <> -1
          then set code i (Goto (mem.lblindex.(a_index)))
          else mem.gotoindex <- (a_index,i)::mem.gotoindex;
        ((i+1),t''))
    | _ -> fail t "Compilation error: Missing a Goto index";;

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
          set code i (Locate (e1, e2, Str_content sl));
          t5)
        | ","::"STR"::t4 ->
          (let (si, t5) = read_int t4 true in
          set code i (Locate (e1, e2, Str_access (si-1)));
          t5)
        (* to come: handling of string expressions *)
        | ","::t4 ->
          (let (e3, t5) = extract_num_expr t4 in
          set code i (Locate (e1, e2, Num_expr e3));
          t5)
        | _ -> fail t "Compilation error: Syntax error in Locate command"
      in
    (i+1,t')
  with
    | Failure _ -> fail t "Compilation error: Syntax error in Locate command";;