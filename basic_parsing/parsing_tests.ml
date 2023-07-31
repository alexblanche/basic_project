(* Combining parser and lexer (testing) *)

(* let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.main Lexer.token lexbuf in
      print_int result; print_newline(); flush stdout
    done
  with Lexer.Eof ->
      exit 0 *)

(* Test for parsing of arithmetical expressions *)
type lexeme = Int of int | Op of string | LPAR | RPAR (* | Function of f*)

let (>>) s1 s2 =
  (s1 = "*" || s1 = "/") && (s2 = "+" || s2 = "-");;

let apply s i1 i2 =
  if s = "+" then i1 + i2
  else if s = "-" then i1 - i2
  else if s = "*" then i1 * i2
  else if s = "/" then i1 / i2
  else failwith "apply: wrong operator"

let rec calculate outq opq =
  match outq, opq with
    | i1::i2::t, (Op s)::opqt -> calculate ((apply s i1 i2)::t) opqt
    | [i], [] -> i
    | _ -> failwith "error"

let rec shunting_yard (lexlist : lexeme list) (output_q : int list) (op_q : lexeme list) =
  match (lexlist,op_q) with
    | [],_ -> calculate output_q op_q
    | (Int i)::t, _ -> shunting_yard t (i::output_q) op_q
    | (Op s)::t, [] -> shunting_yard t output_q [Op s]
    | (Op s)::t, (Op s2)::opq ->
      if s2 >> s
        then (match output_q with
          | i2::i1::outq -> shunting_yard t ((apply s2 i1 i2)::outq) ((Op s)::opq)
          | _ -> failwith "...")
        else shunting_yard t output_q ((Op s)::op_q)
    | (Op s)::t, LPAR::opq -> shunting_yard t output_q ((Op s)::op_q)
    | LPAR::t, _ -> shunting_yard t output_q (LPAR::op_q)
    | RPAR::t, (Op s)::LPAR::opq ->
      (match output_q with
          | i2::i1::outq -> shunting_yard t ((apply s i1 i2)::outq) opq
          | _ -> failwith "something's wrong")
    | RPAR::_, _ -> failwith "Mismatched parentheses";;


  