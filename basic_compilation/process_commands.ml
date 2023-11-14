(* Processing function for each keyword *)

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
      else fail t i "Compilation error: Unexpected Then, does not follow an If statement"
  with
    | Failure _ -> fail t i "Compilation error: Unexpected Then with no opened If statement";;

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
          | _ -> fail t i "Compilation error: Unexpected Else")
      | ("break", j)::stack_t ->
        aux stack_t (("break", j)::br_acc)
      | _ -> fail t i "Compilation error: Unexpected Else with no opened If statement"
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
          | _ -> fail t i "Compilation error: Unexpected IfEnd")
      (* If Then Else IfEnd*)
      (* The Goto index of the If was set by the call to process_else
        that put the ("else", jelse) in mem.stack *)
      | ("else", jelse)::stack_t ->
        (set code jelse (Goto i);
        mem.stack <- List.rev_append br_acc stack_t;
        (i,t))
      | ("break", j)::stack_t ->
        aux stack_t (("break", j)::br_acc)
      | _ -> fail t i "Compilation error: Unexpected IfEnd with no opened If statement"
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
        | _ -> fail t i "Compilation error: Unexpected WhileEnd")
    | ("break", jbr)::stack_t ->
      (set code jbr (Goto (i+1));
      mem.stack <- stack_t;
      process_whileend i t code mem)
    | _ -> fail t i "Compilation error: Unexpected WhileEnd";;


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
    then fail t i "Compilation error: ? cannot set the variable of a For";
  mem.stack <- ("for", i)::mem.stack;
  match t2 with
    | "ASSIGN"::v::"TO"::t3 ->
      (if not (is_var v)
        then fail t i "Compilation error: wrong variable in a For loop";
      let (e2, t4) = extract_num_expr t3 in
      if e2 = QMark
        then fail t i "Compilation error: ? cannot set the bound of a For";
      match t4 with
        | "STEP"::t5 ->
          (let (e3, t6) = extract_num_expr t5 in
          if e3 = QMark
            then fail t i "Compilation error: ? cannot set the bound of a For";
          match t6 with
            | "EOL" :: t7
            | "COLON" :: t7 -> 
              (set code i (For (var_index v, e1, e2, e3, -1));
              (i+1, t7))
            | _ -> fail t i "Compilation error: Syntax error after For loop definition"
          )
        | _ -> (* No Step value, 1 by default *)
          (set code i (For (var_index v, e1, e2, Complex {re = 1.; im = 0.}, -1));
          (i+1, t4))
      )
        
    | _ -> fail t i "Compilation error: Syntax error in a For loop definition";;

let rec process_next i t code mem =
  match mem.stack with
    | ("for", jfor)::stack_t ->
      (mem.stack <- stack_t;
      set code i Next;
      match (!code).(jfor) with
        | For (v,e1,e2,e3,-1) ->
          (set code jfor (For (v,e1,e2,e3,i+1));
          ((i+1),t))
        | _ -> fail t i "Compilation error: Unexpected Next")
    | ("break", jbr)::stack_t ->
      (set code jbr (Goto (i+1));
      mem.stack <- stack_t;
      process_next i t code mem)
    | _ -> fail t i "Compilation error: Unexpected Next";;

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
      | _ -> fail t i "Compilation error: Unexpected LpWhile with no opened Do statement"
  in
  match extract_num_expr t with
    | (QMark, _) -> fail t i "Compilation error: ? cannot be the condition of a Do-LpWhile"
    | (e, t') -> aux mem.stack e t';;

(* Lbl, Goto *)

(* Lbl *)
(* Authorized Goto indices: 0..9, A..Z, r, theta *)
let process_lbl i t code mem =
  match t with
    | a :: "EOL" :: _
    | a :: "COLON" :: _
    | a :: "DISP" :: _
    | [a] ->
      let a_index =
        if is_digit a then Char.code a.[0] - 48
        else if is_letter_var a then var_index a + 10
        else fail t i "Compilation error: Wrong Lbl index"
      in
      (* In Casio Basic, only the first occurrence of a Lbl is considered *)
      if mem.lblindex.(a_index) = -1 then
        mem.lblindex.(a_index) <- i;
      (i, List.tl t)
    | _ -> fail t i "Compilation error: Syntax error in Goto index";;

(* Goto *)
(* Authorized Goto indices: 0..9, A..Z, r, theta *)
let process_goto i t code mem =
  match t with
    | a :: "EOL" :: _
    | a :: "COLON" :: _
    | a :: "DISP" :: _
    | [a] ->
      let a_index =
        if is_digit a then Char.code a.[0] - 48
        else if is_var a then var_index a + 10
        else fail t i "Compilation error: Wrong Goto index"
      in
      (if mem.lblindex.(a_index) <> -1
        then set code i (Goto (mem.lblindex.(a_index)))
        else mem.gotoindex <- (a_index, i)::mem.gotoindex;
      (i+1, List.tl t))
    | _ -> fail t i "Compilation error: Syntax error in Goto index";;

(* Prog *)
let process_prog i t code mem =
  let (lexl, t') = aux_extract_str t in
  let sl =
    List.rev_map
      (fun lex ->
        if String.length lex = 1 &&
          (lex >= "0" && lex <= "9"
          || lex >= "A" && lex <= "Z"
          || lex = " "
          || lex = "."
          || lex = "\126" (* Tilde *)
          || lex = "\039" (* ' *))
          then lex
        else
          try
            List.assoc lex
              [("PLUS", "\137"); ("MINUS", "\153"); ("TIMES", "\169"); ("DIVIDED", "\185"); ("QUOTE", "\034")]
          with
            | Not_found -> fail t i "Compilation error: Incorrect character in Prog name")
      lexl
  in
  let s = String.concat "" sl in
  match t' with
    | "EOL"::t''
    | "COLON"::t'' ->
      (set code i (Prog s);
      ((i+1),t''))
    | "DISP"::_ ->
      (set code i (Prog s);
      ((i+1),t'))
    | [] ->
      (set code i (Prog s);
      ((i+1),[]))
    | _ -> fail t i "Compilation error: Syntax error, a Prog is supposed to be followed by EOL";;

(* Locate *)
let process_locate i t code mem =
  let (e1, t2) = extract_num_expr t in
  try
    if List.hd t2 <> ","
      then fail t i "Compilation error: Syntax error in Locate command";
    let (e2, t3) = extract_num_expr (List.tl t2) in
    let t' =
      match t3 with
        | ","::t4 ->
          let (se, t5) = extract_string_expr t4 in
          (set code i (Locate (e1, e2, se));
          t5)
        | _ -> fail t i "Compilation error: Syntax error in Locate command"
    in
    (i+1,t')
  with
    | Failure _ -> fail t i "Compilation error: Syntax error in Locate command";;

(* DrawStat setup *)
let process_sgph i lexlist code mem : int * string list =
  let (lexl, t') = extract_line lexlist in
  let data =
    match lexl with
      | "EOL" :: q
      | "COLON" :: q
      | "DISP" :: q -> List.rev q
      | _ -> List.rev lexl
  in
  let (sgphi, d2) =
    match data with (* DRAWON, XYLINE, LIST 3, LIST 4, 1, DOT *)
      | "SGPH1" :: q -> (0, q)
      | "SGPH2" :: q -> (1, q)
      | "SGPH3" :: q -> (2, q)
      | _ -> fail lexlist i "Compilation error: Syntax error in Sgph command (Sgph index)"
  in
  let (drawon, d3) =
    match d2 with
      | "DRAWON" :: q -> (true, q)
      | "DRAWOFF" :: q -> (false, q)
      | _ -> fail lexlist i "Compilation error: Syntax error in Sgph command (Drawon/Drawoff)"
  in
  let (stop, style, d4) =
    match d3 with
      | "," :: "SCATTER" :: q -> (false, Scatter, q)
      | "," :: "XYLINE" :: q -> (false, XYLine, q)
      | [] -> (true, Scatter, [])
      | _ -> fail lexlist i "Compilation error: Syntax error in Sgph command (Style)"
  in
  (if stop then
    set code i (Graphic (Drawstat_Setup (sgphi, drawon, None, None, None, None)))
  else
  
  let aux_get_list_index d =
    match d with
      | "," :: "LIST" :: a :: t ->
        if is_digit a then
          let (li, q) = read_int (a::t) true in
          (false, Value (complex_of_int li), q)
        else if is_var a then
          (false, Variable (Var (var_index a)), t)
        else
          fail lexlist i "Compilation error: Syntax error in Sgph command (List index)"
      | [] -> (true, Value (complex_of_float 0.), [])
      | _ -> fail lexlist i "Compilation error: Syntax error in Sgph command (List)"
  in
  let (stop, list1, d5) = aux_get_list_index d4 in
  if stop then
    set code i (Graphic (Drawstat_Setup (sgphi, drawon, Some style, None, None, None)))
  else
  
  let (stop, list2, d6) = aux_get_list_index d5 in
  if stop then
    set code i (Graphic (Drawstat_Setup (sgphi, drawon, Some style, Some list1, None, None)))
  else

  let (stop, mark) =
    match List.rev d6 with 
      | "DOT" :: _ -> (false, DSMDot)
      | "SQUARE" :: _ -> (false, DSMSquare)
      | "CROSS" :: _ -> (false, DSMCross)
      | _ -> (true, DSMDot)
  in

  if stop then
    set code i (Graphic (Drawstat_Setup (sgphi, drawon, Some style, Some list1, Some list2, None)))
  else
    set code i (Graphic (Drawstat_Setup (sgphi, drawon, Some style, Some list1, Some list2, Some mark)))
  );

  let tail =
    match lexl with
      | "DISP" :: _ -> "DISP"::t'
      | _ -> t'
  in
  (i+1, tail);;
