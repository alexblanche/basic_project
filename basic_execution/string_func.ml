(* Functions on strings *)

(*
  STRJOIN
  STRLEN
  STRCMP
  STRSRC
  STRLEFT
  STRRIGHT
  STRMID
  EXPSTR
  EXPPAR
  STRUPR
  STRLWR
  STRINV
  STRSHIFT
  STRROTATE

  (* String expressions *)
  type string_expr =
    | Num_expr of num_expr (* numerical expression *)
    | Str_content of string list (* explicit definition of a string *)
    | Str_access of int (* Str i *)
    | Str_Func of string * string_expr list (* Function applied to several string_expr objects *)
*)

(*  *)
let rec apply_str_func (p : parameters) (fname : string) (sel : string_expr list) : string list =
  match fname, sel with
    | "STRJOIN", [se1; se2] ->
      (match eval_str p se1, eval_str p se2 with
        | Str_content sl1, Str_content sl2 ->
          Str_content (List.rev_append (List.rev sl2) sl1)
        | _ -> failwith "String evaluation error: StrJoin expects two strings as arguments")

    | "STRLEN", [se] ->
      (match eval_str p se with
        | Str_content sl -> Num_expr (List.length sl)
        | _ -> failwith "String evaluation error: StrLen expects one string as argument")

    | "STRCMP", [se1; se2] ->
      (match eval_str p se1, eval_str p se2 with
        | Str_content sl1, Str_content sl2 ->
          let cmp = (fun (c1 : char) (c2 : char) -> if c1<c2 then -1 else if c1 = c2 then 0 else 1) in
          let res_cmp = List.compare cmp (List.rev sl1) (List.rev sl2) in
          Num_expr (complex_of_int res_cmp)
        | _ -> failwith "String evaluation error: StrCmp expects two strings as arguments")

    (*
    | "STRSRC"
    | "STRLEFT"
    | "STRRIGHT"
    | "STRMID"
    | "EXPSTR"
    | "EXPPAR"
    | "STRUPR"
    | "STRLWR"
    | "STRINV"
    | "STRSHIFT"
    | "STRROTATE"
    *)

(* Evaluation function
  returns a string_expr of the form Str_content or Num_expr (used as arguments of functions) *)
and eval_str (p : parameters) (se : string_expr) : string_expr =
  match se with
    | Num_expr e -> Num_expr (Complex (eval_num p e))
    | Str_content _ -> se
    | Str_access si -> p.str.(si)
    | Str_Func (fname, sel) ->
      Str_content (apply_str_func p fname (List.map (fun se -> eval_str p se) sel));;