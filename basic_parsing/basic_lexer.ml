(* Casio Basic lexer *)
(* Reads in G1M files and returns a list of lexemes (stored in basic_type.ml) *)

#use "basic_parsing/file_reader.ml"
#use "basic_parsing/basic_encoding.ml"

(* G1M file format *)
(* 
  Observations:
  - 21 bytes
  - 9 bytes \000
  - 2 bytes: \255\254
  - "PROGRAM"
  - 12 bytes \000
  - 1 byte: \001
  - "system" + \000\000
  - 8 bytes: the name of the Basic program
  - 1 byte: the type of file
    (\001 Basic program, \005 list files, \007 picture, \010 capture)
  - 2 bytes: \000\000
  - 2 bytes: size of the program
  - 13 bytes \000
  [ Beginning of the program ]
  - Padding: some bytes \000
    AAA,     size = 52,   pad = 4
    CHARGPH, size = 644,  pad = 4
    CHAR,    size = 1140, pad = 3
    BBBB,    size = 912,  pad = 4
 *)

(* First attempt at skipping the header for Basic Casio files *)
(* Returns the file in string from, and the index at the start of the Basic program *)
let lexer_basic_prog (file_name : string) : (string * int) =
  let s = file_to_string file_name in

  if (String.sub s (21+9+2) 7) <> "PROGRAM"
    then failwith "lexer_basic_prog: PROGRAM missing";
  if (String.sub s (21+9+2+7+12+1) 8) <> "system\000\000"
    then failwith "lexer_basic_prog: system missing";

  let index_name = 21+9+2+7+12+1+8 in
  (* let prog_name = String.sub s index_name 8 in *)

  if s.[index_name+8] <> '\001'
    then failwith "lexer_basic_prog: not a Basic program";

  (* let size = 256*(Char.code s.[index_name+8+1+2]) + (Char.code s.[index_name+8+1+3]) in *)

  let index_start = index_name+8+1+2+2+13 in

  (* print_endline prog_name;
  print_endline (string_of_int size);
  Array.init 10 (fun i -> s.[index_start+i]); *)
  (s, index_start)
;;

let word_to_pair_of_codes (w : string) : string =
  "("^(string_of_int (Char.code w.[0]))^","^(string_of_int (Char.code w.[1]))^")";;

let prog_to_lexlist (s : string) (istart : int) : string list =
  let n = String.length s in
  let rec aux acc i =
    if i = n || s.[i] = '\000'
      then List.rev acc
      else
        let word =
          if s.[i] = '\127' || s.[i] = '\247' || s.[i] = '\249' ||
            s.[i] = '\229' || s.[i] = '\230' || s.[i] = '\231'
            then String.init 2 (fun j -> s.[i+j])
            else String.init 1 (fun _ -> s.[i])
        in
        if s.[i] >= '0' && s.[i] <= '9' ||
          s.[i] >= 'A' && s.[i] <= 'Z' ||
          s.[i] >= 'a' && s.[i] <= 'z' ||
          s.[i] = ',' || s.[i] = '.' ||
          s.[i] = ' '
          then aux (word::acc) (i+1)
          else
            if word.[0] = '\127' || word.[0] = '\247' || word.[0] = '\249'
              then (* Two-byte word in commands *)
                (try
                  aux ((List.assoc word commands)::acc) (i+2)
                with
                | Not_found ->
                  if List.mem word symbols
                    then aux (word::acc) (i+2)
                    else failwith ("prog_to_lexlist: Unknown word "^word^" "^word_to_pair_of_codes word))
              else (* Two-byte word in symbols *)
                if word.[0] = '\229' || word.[0] = '\230' || word.[0] = '\231'
                  then
                    if List.mem word symbols
                      then aux (word::acc) (i+2)
                      else failwith ("prog_to_lexlist: Unknown symbol "^word^" "^word_to_pair_of_codes word)
                  else (* One-byte word *)
                    (match List.assoc_opt word commands with
                      | None     ->
                        if List.mem word symbols
                          then aux (word::acc) (i+1)
                          else failwith ("prog_to_lexlist: Unknown symbol "^word^" ("^(string_of_int (Char.code word.[0]))^")")
                      | Some lex -> aux (lex::acc) (i+1))
  in
  aux [] istart;;

(* General lexer *)
let lex (file_name : string) : string list =
  let (s,i) = lexer_basic_prog file_name in
  prog_to_lexlist s i;;