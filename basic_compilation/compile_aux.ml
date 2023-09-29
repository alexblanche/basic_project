(* Auxiliary functions for basic_compile *)

(* Fails if the first lexeme of lexlist is not followed by EOL, COLON or DISP *)
let fail_if_not_eol (lexlist : string list) : unit =
  match lexlist with
    | lex :: eol :: t ->
      if eol <> "EOL" && eol <> "COLON" && eol <> "DISP" then
        fail (eol :: t) ("Compilation error: "^lex^" should be followed by Eol")
    | _ -> ();;