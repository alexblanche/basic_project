(* Functions on strings *)

(* Auxiliary function: search for sl2 in sl1 *)
(* Naive implementation: Knuth-Morris-Pratt would be much better *)

(* Returns whether sl2 in a prefix of sl1 *)
let rec is_prefix (sl1 : string list) (sl2 : string list) : bool =
  match sl1, sl2 with
    | h1::t1, h2::t2 -> h1=h2 && (is_prefix t1 t2)
    | _, [] -> true
    | _ -> false;;

(* Returns the position of the first occurrence sl2 in sl1 (starting at 0) *)
(* If sl2 is not present in sl1, the function returns -1 *)
let search (sl1 : string list) (sl2 : string list) : int =
  let rec aux l1 i =
    match l1 with
      | h1::t1 ->
        if is_prefix l1 sl2
          then i
          else aux t1 (i+1)
      | [] -> -1
  in
  aux sl1 0;;

(****************************************************************************************)

(* Application of the string functions *)
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

    | "STRUPR", [se] ->
      (match eval_str p se with
        | Str_content sl ->
          Str_content
            (List.map
              (fun s ->
                if String.length s = 1
                  then String.make 1 (Char.uppercase_ascii s.[0])
                  else s)
              sl)
        | _ -> failwith "String evaluation error: StrUpr expects one string as argument")

    | "STRLWR", [se] ->
      (match eval_str p se with
        | Str_content sl ->
          Str_content
            (List.map
              (fun s ->
                if String.length s = 1
                  then String.make 1 (Char.lowercase_ascii s.[0])
                  else s)
              sl)
        | _ -> failwith "String evaluation error: StrLwr expects one string as argument")

    | "STRLEFT", [se1; se2] ->
      (match eval_str p se1, eval_str p se2 with
        | Str_content sl, Num_expr e ->
          let z = eval_num p e in
          if is_int z && z.re >= 0. then
            (* last_k, because the string in Str_content is stored in reverse order *)
            Str_content (last_k sl (int_of_float z.re))
          else failwith "String evaluation error: StrLeft expects one string and a non-negative integer as arguments" 
        | _ -> failwith "String evaluation error: StrLeft expects one string and a non-negative integer as arguments")

    | "STRRIGHT", [se1; se2] ->
      (match eval_str p se1, eval_str p se2 with
        | Str_content sl, Num_expr e ->
          let z = eval_num p e in
          if is_int z && z.re >= 0. then
            Str_content (first_k sl (int_of_float z.re))
          else failwith "String evaluation error: StrRight expects one string and a non-negative integer as arguments"
        | _ -> failwith "String evaluation error: StrRight expects one string and a non-negative integer as arguments")

    | "STRMID", [se1; se2; se3] ->
      (match eval_str p se1, eval_str p se2, eval_str p se3 with
        | Str_content sl, Num_expr e1, Num_expr e2 ->
          let z1 = eval_num p e1 in
          let z2 = eval_num p e2 in
          if is_int z1 && is_int z2 && z1.re >= 0. && z2.re >= 0. then
            let i1 = int_of_float z1.re in
            let i2 = int_of_float z2.re in
            let n = List.length sl in
            Str_content (last_k (first_k sl (n+1-i1)) i2)
          else failwith "String evaluation error: StrMid expects one string and two non-negative integers as arguments"
        | _ -> failwith "String evaluation error: StrMid expects one string and two non-negative integers as arguments")

    | "STRINV", [se] ->
      (match eval_str p se with
        | Str_content sl -> Str_content (List.rev sl)
        | _ -> failwith "String evaluation error: StrInv expects one string as argument")
    
    | "STRROTATE", [se1; se2] ->
      (match eval_str p se1, eval_str p se2 with
        | Str_content sl, Num_expr e ->
          let z = eval_num p e in
          if is_int z then
            let n = List.length sl in
            let i = int_of_float z.re in
            let j = if i>=0 then i mod n else n + (i mod n) in
            Str_content (List.rev_append (List.rev (last_k sl j)) (first_k sl (n-j)))
          else failwith "String evaluation error: StrRotate expects one string and an integer as arguments"
        | _ -> failwith "String evaluation error: StrRotate expects one string and an integer as arguments")

    | "STRSHIFT", [se1; se2] ->
      (match eval_str p se1, eval_str p se2 with
        | Str_content sl, Num_expr e ->
          let z = eval_num p e in
          if is_int z then
            let n = List.length sl in
            let i = int_of_float z.re in
            let shift_sl =
              if i < 0
                then List.rev_append (List.rev (last_k sl (n+i))) (create_sl (min (-i) n) " ")
                else List.rev_append (create_sl (min i n) " ") (first_k sl (n-i))
            in
            Str_content shift_sl
          else failwith "String evaluation error: StrShift expects one string and an integer as arguments"
        | _ -> failwith "String evaluation error: StrShift expects one string and an integer as arguments")
      
    | "STRSRC", [se1; se2] ->
      (match eval_str p se1, eval_str p se2 with
        | Str_content sl1, Str_content sl2 ->
          let src = search sl1 sl2 in
          if src >= 0
            then Num_expr (Complex (complex_of_int (src + 1)))
            else Num_expr (Complex (complex_of_float 0.))
        | _ -> failwith "String evaluation error: StrCmp expects two strings as arguments")

(* Not implemented:
  | "EXPSTR"
  | "EXPPAR"
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


(** Integration of string functions into arithmetic evaluation **)
let numerical_string_functions = ["STRLEN"; "STRCMP"; "STRSRC"];;