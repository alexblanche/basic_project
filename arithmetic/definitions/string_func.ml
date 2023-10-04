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
(* Returns a string_expr of the form Str_content _ or Num_expr (Complex _) *)
let apply_str_func (p : parameters) (fname : string) (sel : string_expr list) : string_expr =
  match fname, sel with
    | "STRJOIN", [Str_content sl1; Str_content sl2] ->
      Str_content (List.rev_append (List.rev sl2) sl1)

    | "STRLEN", [Str_content sl] ->
      Num_expr (Complex (complex_of_int (List.length sl)))

    | "STRCMP", [Str_content sl1; Str_content sl2] ->
      (* Only works for ascii characters *)
      let cmp = (fun (c1 : string) (c2 : string) -> if c1<c2 then -1 else if c1 = c2 then 0 else 1) in
      let res_cmp = List.compare cmp (List.rev sl1) (List.rev sl2) in
      Num_expr (Complex (complex_of_int res_cmp))

    | "STRUPR", [Str_content sl] ->
      Str_content
        (List.map
          (fun s ->
            if String.length s = 1
              then String.make 1 (Char.uppercase_ascii s.[0])
              else s)
          sl)

    | "STRLWR", [Str_content sl] ->
      Str_content
        (List.map
          (fun s ->
            if String.length s = 1
              then String.make 1 (Char.lowercase_ascii s.[0])
              else s)
          sl)

    | "STRLEFT", [Str_content sl; Num_expr (Complex z)] ->
      if is_int z && z.re >= 0. then
        (* last_k, because the string in Str_content is stored in reverse order *)
        Str_content (last_k sl (int_of_float z.re))
      else failwith "String evaluation error: StrLeft expects one string and a non-negative integer as arguments" 

    | "STRRIGHT", [Str_content sl; Num_expr (Complex z)] ->
      if is_int z && z.re >= 0. then
        Str_content (first_k sl (int_of_float z.re))
      else failwith "String evaluation error: StrRight expects one string and a non-negative integer as arguments"

    | "STRMID", [Str_content sl; Num_expr (Complex z1); Num_expr (Complex z2)] ->
      if is_int z1 && is_int z2 && z1.re >= 0. && z2.re >= 0. then
        let i1 = int_of_float z1.re in
        let i2 = int_of_float z2.re in
        let n = List.length sl in
        Str_content (last_k (first_k sl (n+1-i1)) i2)
      else failwith "String evaluation error: StrMid expects one string and two non-negative integers as arguments"

    | "STRINV", [Str_content sl] ->
      Str_content (List.rev sl)
    
    | "STRROTATE", [Str_content sl; Num_expr (Complex z)] ->
      if is_int z then
        let n = List.length sl in
        let i = int_of_float z.re in
        let j = if i>=0 then i mod n else n + (i mod n) in
        Str_content (List.rev_append (List.rev (last_k sl j)) (first_k sl (n-j)))
      else failwith "String evaluation error: StrRotate expects one string and an integer as arguments"

    | "STRSHIFT", [Str_content sl; Num_expr (Complex z)] ->
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
      
    | "STRSRC", [Str_content sl1; Str_content sl2] ->
      let src = search sl1 sl2 in
      if src >= 0
        then Num_expr (Complex (complex_of_int (src + 1)))
        else Num_expr (Complex (complex_of_float 0.))

    (* Errors *)
    | "STRJOIN", _ -> failwith "String evaluation error: StrJoin expects two strings as arguments"
    | "STRLEN", _ -> failwith "String evaluation error: StrLen expects one string as argument"
    | "STRCMP", _ -> failwith "String evaluation error: StrCmp expects two strings as arguments"
    | "STRUPR", _ -> failwith "String evaluation error: StrUpr expects one string as argument"
    | "STRLWR", _ -> failwith "String evaluation error: StrLwr expects one string as argument"
    | "STRLEFT", _ -> failwith "String evaluation error: StrLeft expects one string and a non-negative integer as arguments"
    | "STRRIGHT", _ -> failwith "String evaluation error: StrRight expects one string and a non-negative integer as arguments"
    | "STRMID", _ -> failwith "String evaluation error: StrMid expects one string and two non-negative integers as arguments"
    | "STRINV", _ -> failwith "String evaluation error: StrInv expects one string as argument"
    | "STRROTATE", _ -> failwith "String evaluation error: StrRotate expects one string and an integer as arguments"
    | "STRSHIFT", _ -> failwith "String evaluation error: StrShift expects one string and an integer as arguments"
    | "STRSRC", _ -> failwith "String evaluation error: StrCmp expects two strings as arguments"

    | s, _ -> failwith ("String evaluation error: Unknown function "^s);;

(* Not (yet) implemented:
  | "EXPSTR"
  | "EXPPAR"
*)

(* List of all string functions *)
let string_func_list =
  ["STRJOIN"; "STRLEN"; "STRCMP";
   "STRUPR"; "STRLWR"; "STRLEFT";
   "STRRIGHT"; "STRMID"; "STRINV";
   "STRROTATE"; "STRSHIFT"; "STRSRC"];;

(* List of string functions that return a numerical value *)
let numerical_string_functions = ["STRLEN"; "STRCMP"; "STRSRC"];;
