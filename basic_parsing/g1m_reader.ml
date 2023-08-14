(* G1M/G2M file reader *)
(* - Splits G1M/G2M files into objects (programs, pictures, ...)
  - Lexer of Basic code (into lexemes stored in basic_encoding.ml)
  - Converts Picture/Capture files into boolean matrices *)

#use "basic_parsing/file_reader.ml"
#use "basic_parsing/basic_encoding.ml"

(* G1M/G2M file format *)
(* 
  - 32 bytes of initial header (for its structure, see g1m_writer.ml)
  - 19 bytes: Data type ("PROGRAM", "LIST 1", "MAT A", "PICTURE 1", "CAPT 1", "STRING 1")
    + padding with \000
  - 1 byte: \001
  - 8 bytes: "system" for programs, "@REV2" for captures, "main" for alpha-mem types
    + padding with \000
  - 8 bytes: the name of the object (name of the program, 1LIST1 (1LIST2...), MAT_M, PICT1, CAPT1, STR1)
    + padding with \000
  - 1 byte: type of file
    (\001 Basic program, \004 string, \005 list, \006 matrix, \007 picture, \010 capture)
  - 4 bytes: size of the data (starting from Default starting point)
  - 3 bytes: \000
  [Default starting point]
  - Data:
    If the object is:
    - a program: 10 bytes \000, then program data
    - a capture: 4 bytes \000\128\000\064, then capture data
    - a picture: picture data
    - a string: string data
  - Padding: (some bytes \000)
    If the object is:
    - a capture: none
    - a string (of length l): 4-(l mod 4) padding bytes
    - a picture:
      * for uncompressed pictures, there are always 2048 bytes after the Default starting point,
        so 1024 of data, 1024 of padding
      * for compressed pictures of l bytes: (4-(l mod 4)) mod 4 padding bytes
    - a program (of length l): 4 - (l mod 4)
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
  else if data_type = "LIST" then c.list <- inf::c.list
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
  let default_istart = index_size + 4 + 3 in
  let index_start =
    default_istart +
      (if data_type = "PROGRAM" then 10
      else if data_type = "PICTURE" || data_type = "STRING" then 0
      else if data_type = "CAPT" then 4
      (* to do: add lists, matrices *)
      else (* default *) 0)
  in

  let index_end = default_istart + size in

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
(* Reading picture/captures files *)

(* Reads the content of the picture at index istart of string s
  (representing a G1M/G2M file) and returns a boolean matrix
  representing the picture *)

(* Purobaz compressed pictures ("Picture 1024")
  https://www.planet-casio.com/Fr/programmes/programme1883-1-picture-1024-purobaz-utilitaires-add-ins.html *)
(* Reads the first nb_bytes bytes of the picture,
  returns a 64*128 matrix
  (16 bytes per line, 1024 = normal picture, < 16 = less than one line) *)
let read_compr_pict (s : string) (istart : int) (nb_bytes : int) : bool array array =
  let m = Array.make_matrix 64 128 false in
  let byte = ref 0 in
  for i = 0 to nb_bytes - 1 do
    byte := Char.code s.[istart+i];
    for j = 7 downto 0 do
      m.(63-i/16).(8*(i mod 16)+j) <- (!byte mod 2) = 1;
      byte := !byte/2
    done;
  done;
  m;;

(* Reads pictures/captures *)
let read_pict (s : string) (istart : int) : bool array array =
  read_compr_pict s istart 1024;;

(*********************************************************************************************)

(* Reading strings *)

(* Reads the content of the string at index istart of string s
  (representing a G1M/G2M file) with length length, and returns
  the string *)
(* I assume that the padding at the end contains only '\000' bytes. *)
let read_str (s : string) (istart : int) (length : int) : string =
  (* The stored string can contain \000 bytes, so we count the number of
    padding bytes and consider that the rest is the string. *)
  let rec aux i =
    if s.[i] <> '\000'
      then i
      else aux (i-1)
  in
  let length = (aux (istart + length - 1) - istart + 1) in
  String.sub s istart length;;


(*********************************************************************************************)

(** Reading lists and matrices **)

(* Converts a string of bytes (numbers between 0 and 255) into an
  array of half_bytes (numbers between 0 and 15) in int array form,
  from index istart and for length bytes *)
let bytes_to_half_bytes (s : string) (istart : int) (length : int) : int array =
	let res = Array.init (2*length)
		(fun i ->
			if i mod 2 = 0
				then (Char.code s.[istart+(i/2)]) / 16
				else (Char.code s.[istart+(i/2)]) mod 16)
	in res;;

(* For testing purposes (digits in lists/matrices are encoded in half-bytes) *)
let display_half_bytes (file_name : string) (istart : int) (length : int) : unit =
	let s = file_to_string file_name in
	let t = bytes_to_half_bytes s istart length in
	let n = Array.length t in
	let i = ref 0 in
	while !i <= n-24 do
		for j = 0 to 23 do
			print_int (t.(!i+j));
			print_char ' '
		done;
		print_newline ();
		i := !i + 24
	done;;

(** Number encoding **)
(*
  12 bytes = 24 half-bytes:
  - 1 half-byte: info
    * 1st bit:
        0 = real or imaginary part,
        1 = real part of a complex
    * 2nd bit: 0 = val >= 0, 1 = val < 0
    * 3rd bit: val < 0 and pow >=  0 (useless for reading)
    * 4th bit:
        0 = val, pow have opposite signs
        1 = val, pow have the same sign
  - 2 half-bytes: power (two digits)
    * if pow >= 0: power
    * if pow < 0: pow + 100
  - 15 half-bytes: digits
  - 3 bytes = 6 half-bytes: padding with \000
*)

(* Reads the content of the list at index istart of string s
  (representing a G1M/G2M file) with length numbers, and returns
  it as an array of floats *)
let read_list (s : string) (istart : int) (length : int) : float array =
  let t = bytes_to_half_bytes s istart (12*length) in
  let extract_numer i =
    let valpos = (t.(24*i) mod 8) < 5 in
    let samesign = t.(24*i) mod 2 = 1 in
    let pow =
      10*t.(24*i+1) + t.(24*i+2) - (if valpos = samesign then 0 else 100)
    in
    let x =
      ((if valpos then "" else "-")
      ^(String.init 15 (fun j -> char_of_int (48+t.(24*i+3+j))))
      ^"e"
      ^string_of_int (pow-14))
    in
    float_of_string x
  in
  Array.init length extract_numer;;

(* Reads the content of the list containing complex numbers at index istart
  of string s (representing a G1M/G2M file) with length numbers, and returns
  it as an array of pairs of floats *)
let read_complex_list (s : string) (istart : int) (length : int) : (float * float) array =
  let t = read_list s istart (2*length) in
  Array.init length (fun i -> (t.(i), t.(i+length)));;

(* Reads the content of the matrix at index istart of string s
  (representing a G1M/G2M file) with row rows and col columns,
  and returns it as an matrix of floats *)
let read_matrix (s : string) (istart : int) (row : int) (col : int) : float array array =
  let t = read_list s istart (row*col) in
  let m = Array.make_matrix row col 0. in
  for j = 0 to row-1 do
    for i = 0 to col-1 do
      m.(j).(i) <- t.(j*col + i)
    done;
  done;
  m;;

(* For the moment, for lists and matrices, the actual index to read from is istart+16, as
  given by get_content. *)

(*********************************************************************************************)

(* General g1m reader function *)

#use "basic_parsing/project_type.ml"

(* Returns the content of each object of the G1m/G2M file *)
let g1m_reader (s : string) : project_content =
  let c = get_content s in
  
  let prog_list =
    List.rev_map
      (fun (name, _, istart, _) -> (name, prog_to_lexlist s istart))
      c.prog
  in

  (* List indices range from 1 to 26 *)
  (* In case of a complex list of n complex numbers,
    2n floats are stored, the n real parts first, then
    the n imaginary parts *)
  let list_array = Array.make 26 (true, [||]) in
  List.iter
    (fun (name, length, istart, _) ->
      (* name = 1LISTX or 1LISTXX *)
      let index_length = String.length name - 5 in
      let casio_index =
        int_of_string (String.sub name 5 index_length)
      in
      let list_length =
        256*(Char.code s.[istart+8])
        + (Char.code s.[istart+9])
      in
      let real = (length-16)/12 = list_length in
      list_array.(casio_index-1) <-
        (real,
        read_list s (istart + 16) (if real then list_length else 2*list_length));
    )
    c.list;
  
  (* Matrix indices range from A to Z *)
  (* In case of a complex matrix of row*col complex numbers,
    2*row rows (of length col) are stored, the row real parts first,
    then the row imaginary parts *)
  let mat_array = Array.make 26 (true, [||]) in
  List.iter
    (fun (name, length, istart, _) ->
      (* name = MAT_X *)
      let casio_index =
        Char.code name.[4] - 65
      in
      let row =
        256*(Char.code s.[istart+8])
        + (Char.code s.[istart+9])
      in
      let col =
        256*(Char.code s.[istart+10])
        + (Char.code s.[istart+11])
      in
      let real = (length-16)/12 = row*col in
      mat_array.(casio_index) <-
        (real,
        read_matrix s (istart + 16) (if real then row else 2*row) col);
    )
    c.mat;

  (* Picture indices range from 1 to 20 *)
  let pict_array = Array.make 20 (0, [||]) in
  List.iter
    (fun (name, length, istart, _) ->
      (* name = PICTX or PICTXX *)
      let index_length = String.length name - 4 in
      let casio_index =
        int_of_string (String.sub name 4 index_length)
      in
      pict_array.(casio_index-1) <-
        (length,
        read_compr_pict s istart (min length 1024));
    )
    c.pict;

  (* Capture indices range from 1 to 20 *)
  let capt_array = Array.make 20 [||] in
  List.iter
    (fun (name, length, istart, _) ->
      (* name = CAPTX or CAPTXX *)
      let index_length = String.length name - 4 in
      let casio_index =
        int_of_string (String.sub name 4 index_length)
      in
      capt_array.(casio_index-1) <- read_pict s istart;
    )
    c.capt;

  (* String indices range from 1 to 20 *)
  let str_array = Array.make 20 "" in
  List.iter
    (fun (name, length, istart, _) ->
      (* name = STRX or STRXX *)
      let index_length = String.length name - 3 in
      let casio_index =
        int_of_string (String.sub name 3 index_length)
      in
      str_array.(casio_index-1) <- read_str s istart length;
    )
    c.str;

  {
    prog = prog_list;
    list = list_array;
    mat = mat_array;
    pict = pict_array;
    capt = capt_array;
    str = str_array
  }
;;