(* Auxiliary functions for basic_compile *)

(* Fails if the first lexeme of lexlist is not followed by EOL, COLON or DISP *)
let fail_if_not_eol (lexlist : string list) : unit =
  match lexlist with
    | lex :: eol :: t ->
      if eol <> "EOL" && eol <> "COLON" && eol <> "DISP" then
        fail (eol :: t) ("Compilation error: "^lex^" should be followed by Eol")
    | _ -> ();;

(* Returns the list l without the first consecutive "break" elements *)
let rec skip_breaks (l : (string * int) list) =
  match l with
    | ("break", _)::t -> skip_breaks t
    | _ -> l;;

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