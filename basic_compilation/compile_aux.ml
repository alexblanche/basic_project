(* Auxiliary functions for basic_compile *)

(* Fails if the first lexeme of lexlist is not followed by EOL, COLON or DISP *)
let fail_if_not_eol (lexlist : string list) (i : int) : unit =
  match lexlist with
    | lex :: eol :: t ->
      if eol <> "EOL" && eol <> "COLON" && eol <> "DISP" then
        fail (eol :: t) i ("Compilation error: "^lex^" should be followed by Eol")
    | _ -> ();;

(* Returns the list l without the first consecutive "break" elements *)
let rec skip_breaks (l : (string * int) list) =
  match l with
    | ("break", _)::t -> skip_breaks t
    | _ -> l;;