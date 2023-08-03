(* Casio Basic lexer *)
(* Reads in G1M files and returns a list of lexemes (stored in basic_type.ml) *)

(* Work in progress *)
(*
  What works:
  - Lexing a G1M containing one Basic program
  - Splitting a G1M containing only Basic programs into a list of
    (name, size, index where the program starts) for each program
  
  To do:
  - Lexing each program (easy)
  - Splitting a G1M containing any list of objects (Basic program, List, Mat, Alpha-mem, Capture, Picture...)
    into a list of (name, size, index)
  - (in another program) Reading the data of the objects that are not programs
    *)

#use "basic_parsing/file_reader.ml"
#use "basic_parsing/basic_encoding.ml"

(* G1M file format *)
(* 
  Observations:
  - 21 bytes
  - 9 bytes \000
  - 2 bytes: \255\25*
  - "PROGRAM"
  - 12 bytes \000
  - 1 byte: \001
  - "system" + \000\000
  - 8 bytes: the name of the Basic program
    (if the title is shorter, padding with \000)
  - 1 byte: the type of file
    (\001 Basic program, \005 list files, \007 picture, \010 capture)
  - 4 bytes: size of the program
  - 13 bytes \000
  [ Beginning of the program ]
  - Padding: some bytes \000
 *)

(* First attempt at skipping the header for Basic Casio files *)
(* Returns the file in string from,
   the index at the start of the Basic program
   and the size of the program *)
let lexer_basic_prog (file_name : string) : (string * int * int) =
  let s = file_to_string file_name in

  if (String.sub s (21+9+2) 7) <> "PROGRAM"
    then failwith "lexer_basic_prog: PROGRAM missing";
  if (String.sub s (21+9+2+7+12+1) 8) <> "system\000\000"
    then failwith "lexer_basic_prog: system missing";

  let index_name = 21+9+2+7+12+1+8 in
  (* let prog_name = String.sub s index_name 8 in *)

  if s.[index_name+8] <> '\001'
    then failwith "lexer_basic_prog: not a Basic program";

  let size = 256*(Char.code s.[index_name+8+1+2]) + (Char.code s.[index_name+8+1+3]) in

  let index_start = index_name+8+1+2+2+13 in

  (* print_endline prog_name;
  print_endline (string_of_int size);
  Array.init 10 (fun i -> s.[index_start+i]); *)
  (s, index_start, size)
;;

let word_to_pair_of_codes (w : string) : string =
  "("^(string_of_int (Char.code w.[0]))^","^(string_of_int (Char.code w.[1]))^")";;

(* Converts the Basic program in string s starting at index istart
  into a list of lexemes  *)
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

(* General lexer of a simple Basic program *)
let lex (file_name : string) : string list =
  let (s,i,_) = lexer_basic_prog file_name in
  prog_to_lexlist s i;;


(*********************************************************)
(* Splitting a G1M file into Basic programs *)

(* Returns the name and (alleged) size of the first Basic program in string s
  after index iprog, when i prog is the index at the beginning of the "PROGRAM" part *)
let get_prog_name_size (s : string) (iprog : int) : string * int =
  let index_name = iprog+7+12+1+8 in
  let prog_name = String.sub s index_name 8 in

  if s.[index_name+8] <> '\001'
    then failwith "get_prog_name_size: not a Basic program";

  let index_size = index_name+8+1 in
  let size =
    (Char.code s.[index_size+3])
    + 256 *
      (
        (Char.code s.[index_size+2])
        + 256 *
          (
            (Char.code s.[index_size+1])
            + 256 * (Char.code s.[index_size+0])
          )
      ) in

  (prog_name, size);;


(* When the G1M represented by string s is made up of basic programs
  (and no pictures, List, ...),
  Returns the list of (name, size, istart) of each program in the G1M file
  represented by the string s, if this file is made up of basic programs
  (and no pictures, List, ...).

  name: name of the program (max 8 characters)
  size: size value of the program (as present in the header)
  istart: index of the start of the program
  The program is actually (size-10) bytes long. *)
let basic_split (s : string) : (string * int * int) list =
  let n = String.length s in
  (* Initial header (first "PROGRAM" part) *)
  let initial_header_size = 21+9+2 in
  (* Program header *)
  let prog_header_size = 7+12+1+8+8+1+4+13 in
  let rec aux acc i =
    if i >= n
      then List.rev acc
      else
        let (name, size) = get_prog_name_size s i in
        let istart = i + prog_header_size in
        aux ((name, size, istart)::acc) (istart+size-10)
  in
  aux [] initial_header_size;;