(* G1M/G2M file reader *)
(* - Splits G1M/G2M files into objects (programs, pictures, ...)
  - Lexer of Basic code (into lexemes stored in basic_encoding.ml)
  - Converts Picture/Capture files into boolean matrices *)

#use "basic_parsing/file_reader.ml"
#use "basic_parsing/basic_encoding.ml"

(* G1M/G2M file format *)
(* 
  - 32 bytes of initial header (for its structure, see g1m_writer.ml)
  - 19 bytes: Data type ("PROGRAM", "LISTFILE", "MAT", "PICTURE", "CAPT", (STR for string?))
    + padding with \000
  - 1 byte: \001 (different for list/mat?)
  - 8 bytes: ("system" + 2 \000) for programs, ("main" + 4 \000) for alpha-mem types
  - 8 bytes: the name of the object (name of the program, PICT1, MAT_M, ...)
    (if the title is shorter, padding with \000)
  - 1 byte: type of file
    (\001 Basic program, \005 list files, \007 picture, \010 capture...)
  - 4 bytes: size of the program
  - 7 bytes \000
  [Default starting point]
  (if the object is:
    - a program: 6 bytes \000, then beginning of the program)
    - a capture: beginning of the image data
    - a picture: 12 bytes \000, then beginning of the image data)
  - Padding: some bytes \000
 *)
(* Size of each object: size - 4 (starting from Default starting point (regardless of data type))
   size = 4 + iend - idep + 6(for a prog file) *)

(* Number of padding bytes

  Empirical formula, for a program:
  when l is the length of the data describing the object (in bytes),
  there there are 4-(n+10) mod 4 bytes \000 of padding,
  i.e. as many bytes as it takes to make n+4+6 = 0 mod 4
  (+4 because the actual size is size-4, 6 is the specific offset for programs)
*)

(*********************************************************************************************)

(* Splitting a G1M file into objects (program, list, matrix, picture, capture, string) *)

(* Info: (name, size, index)
  Given a G1M file in string form:
  name: name of the program, the list, etc. (max 8 characters)
  size: size in the string
  index_start: index of the beginning of the data in the string
  index_end: index of the beginning of the next object
             (= size of s if it is the last object *)
type info = string * int * int * int

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
  let name = String.sub s index_name (min 8 (index_name_end - index_name)) in

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
  
  (* The starting index differs depending on the data type *)
  let default_istart = index_size + 4 + 7 in
  let index_start =
    if data_type = "PROGRAM" then default_istart + 6
    else if data_type = "PICTURE" then default_istart + 12
    else if data_type = "CAPT" then default_istart
    (* to do: add more types *)
    else index_size + 4 + 13 (* default *)
  in

  let index_end = default_istart + size - 4 in

  (data_type, (name, size, index_start, index_end));;


(* Given a string s representing a G1M/G2M file,
  returns the content of the file, i.e. the info of each object.
  A program is actually (size-10) bytes long. *)
let get_content (s : string) : content =
  let n = String.length s in
  (* Initial header (first data type ("PROGRAM", "PICTURE") part) *)
  let initial_header_size = 32 in
  let cont = empty_content () in
  let rec aux i =
    if i >= n
      then reverse_all_fields cont
      else
        (let (data_type, (name, size, istart, iend)) = get_info s i in
        add_info cont data_type (name, size, istart, iend);
        aux iend)
  in
  aux initial_header_size;
  cont;;

(*********************************************************************************************)
(** Lexing Basic programs **)

let word_to_pair_of_codes (w : string) : string =
  "("^(string_of_int (Char.code w.[0]))^","^(string_of_int (Char.code w.[1]))^")";;

(* Converts the Basic program in string s starting at index istart
  into a list of lexemes *)
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


(*********************************************************************************************)
(* Reading picture files *)

(* Reads the content of the picture/capture at index istart
  of string s (representing a G1M/G2M file) and returns a boolean
  matrix representing the picture *)
let read_pict (s : string) (istart : int) : bool array array =
  let m = Array.make_matrix 64 128 false in
  let byte = ref 0 in
  for i = 0 to 1023 do
    byte := Char.code s.[istart+i];
    for j = 7 downto 0 do
      m.(63-i/16).(8*(i mod 16)+j) <- (!byte mod 2) = 1;
      byte := !byte/2
    done;
  done;
  m;;


(* (* Tests *)
#use "picture_editor/picture_drawer.ml"

let read_all_pict (s : string) : unit =
  let c = get_content s in
  List.iter (fun (_,_,i,_) -> let m = read_pict s i in view_matrix m) c.pict;;

let read_all_capt (s : string) : unit =
  let c = get_content s in
  List.iter (fun (_,_,i,_) -> let m = read_pict s i in view_matrix m) c.capt;; *)

(*********************************************************************************************)

(* To do: read other objects (lists, mat, str)
  Search if other objects can be put into G1M/G2M files *)