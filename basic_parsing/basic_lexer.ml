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
(* Splitting a G1M file into elements (program, list, matrix, picture, capture, string) *)

(* Info: (name, size, index)
  Given a G1M file in string form:
  name: name of the program, the list, etc. (max 8 characters)
  size: size in the string
  index: index of the beginning of the data in the string *)
type info = string * int * int

type content =
  {
    mutable prog : info list;
    mutable list : info list;
    mutable mat : info list;
    mutable pict : info list;
    mutable capt : info list;
    mutable str : info list
  }

(* Returns an object of type content with empty fields *)
let empty_content () : content =
  {prog = []; list = []; mat = []; pict = []; capt = []; str = []};;

(* Reverses the lists in all the fields in content c *)
let reverse_all_fields (c : content) : unit =
  c.prog <- List.rev c.prog;
  c.list <- List.rev c.list;
  c.mat <- List.rev c.mat;
  c.pict <- List.rev c.pict;
  c.capt <- List.rev c.capt;
  c.str <- List.rev c.str;;

(* Add the info to right field of content c *)
let add_info (c : content) (data_type : string) (inf : info) : unit =
  if data_type = "PROGRAM" then c.prog <- inf::c.prog
  else if data_type = "LISTFILE" then c.list <- inf::c.list
  else if data_type = "MAT" then c.mat <- inf::c.mat
  else if data_type = "PICTURE" then c.pict <- inf::c.pict
  else if data_type = "CAPT" then c.capt <- inf::c.capt
  else if data_type = "STRING" then c.str <- inf::c.str (* To do: check if this type exists *)
  else failwith ("add_info: unknown data type "^data_type);;
  

(* Returns the data type ("PROGRAM", "PICTURE", ...) in string form, and the info in string s
  after index ihead, when ihead is the index at the beginning of the data type part *)
let get_info (s : string) (ihead : int) : string * info =
  let index_data_type_end_1 = String.index_from_opt s ihead '\000' in
  let index_data_type_end_2 = String.index_from_opt s ihead ' ' in
  let index_data_type_end =
    match index_data_type_end_1, index_data_type_end_2 with
      | Some d1, Some d2 -> min d1 d2
      | Some d1, None -> d1
      | None, _ -> failwith "get_info: file has no '\000' character"
  in
  let data_type = String.sub s ihead (index_data_type_end-ihead) in

  let index_name = ihead+19+1+8 in
  let index_name_end =
    match String.index_from_opt s index_name '\000' with
      | None -> 8
      | Some i -> i
  in
  let name = String.sub s index_name (index_name_end - index_name) in

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

  (data_type, (name, size, index_size+4+13));;


(* Given a string s representing a G1M/G2M file,
  returns the content of the file, i.e. the info of each object.
  A program is actually (size-10) bytes long. *)
let get_content (s : string) : content =
  let n = String.length s in
  (* Initial header (first data type ("PROGRAM", "PICTURE") part) *)
  let initial_header_size = 21+9+2 in
  let cont = empty_content () in
  let rec aux i =
    if i >= n
      then reverse_all_fields cont
      else
        (let (data_type, (name, size, istart)) = get_info s i in
        add_info cont data_type (name, size, istart);
        aux (istart+size-10))
  in
  aux initial_header_size;
  cont;;

(*********************************************************)
(* Reading picture files *)

(* Reads the content of the picture at index istart of string s
  (representing a G1M/G2M file) and returns a boolean matrix
  representing the picture *)
let read_pict (s : string) (istart : int) : bool array array =
  let m = Array.make_matrix 64 128 false in
  let byte = ref 0 in
  for i = 0 to 1023 do
    byte := Char.code s.[istart+6+i];
    for j = 7 downto 0 do
      m.(63-i/16).(8*(i mod 16)+j) <- (!byte mod 2) = 1;
      byte := !byte/2
    done;
  done;
  m;;


(* Tests *)
#use "picture_editor/picture_drawer.ml"

let read_all_pict (s : string) : unit =
  let c = get_content s in
  List.iter (fun (_,_,i) -> let m = read_pict s i in view_matrix m) c.pict;;
