(* G1M/G2M file writer *)

(* Creates the file file_name and writes the string s in it *)
let write_file (file_name : string) (s : string) : unit =
  let oc = open_out file_name in
  output_string oc s;
  close_out oc;;

(*******************************************************************)


#use "basic_parsing/project_type.ml"
#use "basic_parsing/basic_encoding.ml"

(** Headers **)

(** Initial header **)

(*
  Structure of the initial header:

  The bits of the header are inverted (0 -> 1; 1 -> 0).
  Inverted bytes:
  - 8 bytes: "USBPower"
  - 1 byte (file type): if g1m \049, if g2m \098
  - 5 bytes: \000\016\000\016\000
  - 1 byte (first size control byte): ((Byte at position 19) + 65) mod 256
  - 1 byte: \001
  - 4 bytes: file size
  - 1 byte (second size control byte): ((Byte at position 19) + 184) mod 256
  - 1 byte (type control byte): if g1m \255, if g2m \254 
  - 8 bytes \255
  - 2 bytes: number of objects contained in the file
  Then apply (fun i -> 255-i) to the bytes.

  Source: https://media.taricorp.net/prizm/simon_chm/fxReverse2x.pdf

*)

(* Returns the initial header of the file of type file_type ("g1m" or "g2m", to do: "g2r"),
  of length length and containing nb_obj objects *)
let init_header (file_type : string) (length : int) (nb_obj : int) : string =
  let t = [|85; 83; 66; 80; 111; 119; 101; 114; 49; 0; 16; 0; 16; 0;
  0; 1; 0; 0; 0; 0; 0; 255; 255; 255; 255; 255; 255; 255; 255; 255;
  0; 0|] in
  (* File type *)
  (* By default, 49 for "g1m" *)
  if file_type = "g2m"
    then t.(8) <- 98;
  (* File size *)
  let l = ref length in
  for i = 0 to 3 do
    t.(19-i) <- !l mod 256;
    l := !l / 256
  done;
  (* First control byte *)
  t.(14) <- (t.(19) + 65) mod 256;
  (* Second control byte *)
  t.(20) <- (t.(19) + 184) mod 256;
  (* Type control byte *)
  if file_type = "g2m"
    then t.(21) <- 254;
  (* Number of objects *)
  t.(30) <- nb_obj / 256;
  t.(31) <- nb_obj mod 256;
  (* Header string *)
  String.init 32 (fun i -> Char.chr (255-t.(i)));;

(** Subheaders **)

(* Returns the subheader for an object of type obj_type, with given name and length) *)
(* obj_type belongs to {"PROGRAM", "LIST", "MAT", "PICTURE", "CAPT", "STRING"}
  name is "1" for List, Picture, Capt, String numero 1,
  "A" for Mat A, or the name of the program (max 8 characters) *)
let obj_subheader (obj_type : string) (name : string) (length : int) : string =
  (* Checking obj_type *)
  if (obj_type <> "PROGRAM" &&
    obj_type <> "LIST" &&
    obj_type <> "MAT" &&
    obj_type <> "PICTURE" &&
    obj_type <> "CAPT" &&
    obj_type <> "STRING")
    then
      failwith "obj_subheader: incorrect obj_type";
  
  (* Checking length of the name *)
  if (String.length name > 8)
    then
      failwith "obj_subheader: incorrect name (max length is 8 characters)";

  (* 19 bytes: data type + padding with '\000' *)
  let data_type =
    if obj_type = "PROGRAM"
      then
        obj_type^(String.make 12 '\000')
      else
        obj_type^" "^name^(String.make (19 - (String.length obj_type + String.length name + 1)) '\000')
  in

  (* 8 bytes: folder + padding wtih '\000' *)
  let folder =
    if obj_type = "PROGRAM" then "system\000\000"
    else if obj_type = "CAPT" then "@REV2\000\000\000"
    else "main\000\000\000\000"
  in

  (* 8 bytes: name + padding with '\000' *)
  let name_part =
    if obj_type = "PROGRAM"
      then name^(String.make (8-String.length name) '\000')
      else if obj_type = "CAPT" || obj_type = "PICTURE"
        then (String.sub obj_type 0 4)^name^(String.make (4-String.length name) '\000')
        else if obj_type = "MAT"
          then "MAT_"^name^(String.make 3 '\000')
          else if obj_type = "LIST"
            then "1LIST"^name^(String.make (3-String.length name) '\000')
            else (* String *) "STR"^name^(String.make (5-String.length name) '\000')
  in

  (* 1 byte *)
  let file_type =
    List.assoc obj_type
      [("PROGRAM", "\001");
      ("STRING", "\004");
      ("LIST", "\005");
      ("MAT", "\006");
      ("PICTURE", "\007");
      ("CAPT", "\010")]
  in

  (* 4 bytes *)
  let size =
    let l = ref (256*length) in
    let t = Array.init 4 (fun _ -> l := !l/256; !l mod 256) in
    String.init 4 (fun i -> char_of_int (t.(3-i)))
  in

  (* Default: 7 bytes *)
  let padding =
    String.make 3 '\000'
  in

  (* Subheader string *)
  data_type ^ "\001" ^ folder ^ name_part ^ file_type ^ size ^ padding;;


(*******************************************************************)

(** Conversions to binary **)

(* Conversion from boolean matrix to binary encoding of a picture *)
(* The matrix is assumed to have size 64*128.
  Only the first nb_bytes/16 lines are read
  (if nb_bytes < 16, only the first 8*nb_bytes pixels are read) *)
let bool_mat_to_pict_bin (m : bool array array) (nb_bytes : int) : string =
  let aux i =
    let res = ref 0 in
    for j = 0 to 7 do
      res := 2 * !res + (if m.(63-i/16).(8*(i mod 16)+j) then 1 else 0)
    done;
    char_of_int (!res)
  in
  String.init nb_bytes aux;;

(* Conversion from boolean matrix to binary encoding a capture *)
(* The matrix is assumed to have size 64*128. *)
let bool_mat_to_capt_bin (m : bool array array) : string =
  bool_mat_to_pict_bin m 1024;;

(*******************************************************************)

(** File generation **)

(* For testing purposes *)
(* Test: generates a g1m file with name file_name, containing
  one picture, with number number, based on boolean matrix m *)
(* Only the first nb_bytes bytes are read.
  - For uncompressed pictures: nb_bytes = 2048
  - For compressed pictures: 4 <= nb_bytes <= 1024 (multiple of 4) *)
let write_pict (number : int) (m : bool array array) (nb_bytes : int) (file_name : string) =
  let data = bool_mat_to_pict_bin m (min nb_bytes 1024) in
  let subh = obj_subheader "PICTURE" (string_of_int number)
    (if nb_bytes = 2048 then 2048 else nb_bytes + (4-(nb_bytes mod 4)) mod 4) in
  let length = (String.length subh) + nb_bytes
    + (if nb_bytes = 2048 then 0 else (4-(nb_bytes mod 4)) mod 4) + 32 in
  let head = init_header "g1m" length 1 in
  let padding = String.make (if nb_bytes = 2048 then 1024 else (4-(nb_bytes mod 4)) mod 4) '\000' in
  let file_content = head^subh^data^padding in
  write_file file_name file_content;;

(* Returns the binary content of the pictures in pict_array.
  Each element of pict_array is a pair (nb_bytes,m), where m is a
  boolean matrix and nb_bytes is the number of bytes read
    - For compressed pictures: 4 <= nb_bytes <= 1024 (multiple of 4),
    - For uncompressed pictures: nb_bytes = 2048 *)
let pict_bin (pict_array : (int * (bool array array)) array) : string =
  let bin i (nb_bytes,m) =
    if nb_bytes = 0
      then ""
      else
        let subh = obj_subheader "PICTURE" (string_of_int (i+1)) nb_bytes in
        let padding =
          String.make (if nb_bytes = 2048 then 1024 else (4-(nb_bytes mod 4)) mod 4) '\000'
        in
        subh^(bool_mat_to_pict_bin m (min nb_bytes 1024))^padding
  in
  let data_l = Array.to_list (Array.mapi bin pict_array) in
  String.concat "" data_l;;

(* Returns the binary content of the captures in capt_array.
  Each element of capt_array is a 64*128 boolean matrix *)
let capt_bin (capt_array : (bool array array) array) : string =
  let bin i m =
    if m = [||]
      then ""
      else
        let subh = obj_subheader "CAPT" (string_of_int (i+1)) (4 + 1024) in
        subh^"\000\128\000\064"^(bool_mat_to_capt_bin m)
  in
  let data_l = Array.to_list (Array.mapi bin capt_array) in
  String.concat "" data_l;;

(* Returns the binary content of the strings in str_array *)
let str_bin (str_array : string array) : string =
  let bin i str =
    if str = "" (* How do I differentiate empty strings and non-existent ones? *)
      then ""
      else
        let len = String.length str in
        let padding_size = 4 - (len mod 4) in
        let subh = obj_subheader "STRING" (string_of_int (i+1)) (len + padding_size) in
        subh^str^(String.make padding_size '\000')
  in
  let data_l = Array.to_list (Array.mapi bin str_array) in
  (String.concat "" data_l);;


(* Programs conversion to binary *)

(* Unused *)
(* (* Equivalent to List.mapi, but is tail-recursive *)
let tr_list_mapi (f : int -> 'a -> 'b) (l : 'a list) : 'b list =
  let rec aux acc i l =
    match l with
      | a::t -> aux ((f i a)::acc) (i+1) t
      | [] -> acc
  in
  List.rev (aux [] 0 l);; *)

(* Converts a list of lexemes into a list of encodings *)
let lexlist_to_bin (lexlist : string list) : string list =
  let rec bin lex =
    try
      let c = Char.code lex.[0] in
      if (String.length lex = 1) &&
        (c >= 65 && c <= 90
        || c >= 48 && c <= 57
        || c = 46 || c = 44
        || c = 32)
          then lex
          else
            let (enco, _) = List.find (fun (_, s) -> s = lex) commands in
            enco
    with
      | Not_found ->
        if List.mem lex symbols
          then lex
          else failwith ("lexlist_to_bin: Unknown lexeme "^lex)
  in
  List.rev (List.rev_map bin lexlist);;

(* Returns the binary content of the programs in prog_array.
  Each program is a list of lexemes. *)
let prog_bin (prog_list : program list) : string =
  let bin (name, lexlist) =
    let encolist = lexlist_to_bin lexlist in
    let data = String.concat "" encolist in
    let len = String.length data in
    let padding_size = 4 - ((len + 10) mod 4) in
    let subh = obj_subheader "PROGRAM" name (10 + len + padding_size) in
    subh^(String.make 10 '\000')^data^(String.make padding_size '\000')
  in
  let data_l = List.map bin prog_list in
  (String.concat "" data_l);;

(*********************************************************************************************)

(** Lists/matrices generation **)

(* List data structure
  After the subheader:
  - 8 bytes: random data (we write \000 bytes instead)
  - 2 bytes: length of the list (in number of elements)
  - 6 bytes: random data (we write \000 bytes instead)
  - 12*length bytes of data (each number is encoded on 12 bytes) *)
(* The "random" bytes seem empirically random and do not appear to be read by the calculator. *)

(* Encoding of Casio numbers *)
(*
  12 bytes = 24 half-bytes:
  - 1 half-byte: info
    * 1st bit:
        0 = real or imaginary part,
        1 = real part of a complex
    * 2nd bit: 0 = val >= 0, 1 = val < 0
    * 3rd bit: 1 = val < 0 and pow >=  0 (useless for reading)
    * 4th bit:
        0 = val, pow have opposite signs
        1 = val, pow have the same sign
  - 2 half-bytes: power (two digits)
    * if pow >= 0: power
    * if pow < 0: pow + 100
  - 15 half-bytes: digits
  - 3 bytes = 6 half-bytes: padding with \000
*)
(* real_part = true if the number is the real part of a complex number *)
let float_to_bin (x : float) (real_part : bool) : string =
  let pow = Float.floor (Float.log10 (Float.abs x)) in
  (* Info half-byte value *)
  let info =
    8*(if real_part then 1 else 0)
    + 4*(if x >= 0. then 0 else 1)
    + 2*(if x < 0. && pow >= 0. then 1 else 0)
    + 1*(if x < 0. && pow < 0. || x >= 0. && pow >= 0. then 1 else 0)
  in
  (* Significant part of x *)
  let y = ref (Float.to_int (x *. 10.**(-.pow+.14.))) in
  (* Array of the digits of the signigicant part of x *)
  let t = Array.init 15
    (fun i -> let last_digit = !y mod 10 in y := !y / 10; Int.abs last_digit)
  in
  (* Power as written in the second and third half-bytes *)
  let written_pow =
    if pow >= 0.
      then Float.to_int pow
      else (Float.to_int pow)+100
  in
  (* Final string *)
  (String.init 2
    (fun i ->
      if i=0
        then (Char.chr (16*info+written_pow/10))
        else (Char.chr (16*(written_pow mod 10)+t.(14)))))
  ^(String.init 7 (fun i -> Char.chr (16*t.(14-(2*i+1))+t.(14-(2*i+2)))))
  ^"\000\000\000"
;;

(* Returns the binary content of the lists in list_array *)
(* Each list is stored as (complex, t), where complex is a boolean
  indicating if the list contains at least one complex number,
  and t is an array containing the numbers of the list.
  If the list contains only real numbers, the array t has the same
  length as the list, otherwise, t has twice its length, and contains
  the real parts first, then the imaginary parts. *)
(* In a list that contains complex numbers, the first bit of a real number
  is 0, and its imaginary part does not seem to be read (here we store
  the real number 0). *)
let list_bin (list_array : (bool * (float array)) array) : string =
  let bin i (complex,t) =
    if t = [||]
      then ""
      else
        (* If complex, then there are length/2 complex numbers,
          otherwise length real numbers *)
        let length = Array.length t in
        let actual_length =
          if complex
            then length/2
            else length
        in
        let subh = obj_subheader "LIST" (string_of_int (i+1)) (16+12*length) in
        let pre_data =
          (String.make 8 '\000')
          ^(String.make 1 (Char.chr (actual_length/256)))
          ^(String.make 1 (Char.chr (actual_length mod 256)))
          ^(String.make 6 '\000')
        in
        let data_l =
          List.init length
            (fun i ->
              float_to_bin t.(i)
                (complex && i < actual_length && t.(i+actual_length) <> 0.))
        in
        subh^pre_data^(String.concat "" data_l)
  in
  let data_l = Array.to_list (Array.mapi bin list_array) in
  (String.concat "" data_l);;

(* Converts a matrix into an array (row by row) *)
let matrix_to_array (m : 'a array array) : 'a array =
  if m = [||]
    then [||]
    else 
      (let row = Array.length m in
      let col = Array.length m.(0) in
      let t = Array.make (row*col) m.(0).(0) in
      for j = 0 to row-1 do
        for i = 0 to col-1 do
          t.(j*col + i) <- m.(j).(i)
        done;
      done;
      t)
;;


(* Returns the binary content of the matrices in mat_array *)
(* Each matrix is stored as (complex, m), where complex is a boolean
  indicating if the matrix contains at least one complex number,
  and m is a matrix containing the numbers of the matrix.
  If the matrix contains only real numbers, the matrix m has the same
  number of rows as the represented matrix, otherwise, m has twice its
  number of rows, and contains the rows of the real parts first, then
  the rows of the imaginary parts. *)
(* In a matrix that contains complex numbers, the first bit of a real number
  is 0, and its imaginary part does not seem to be read (here we store
  the real number 0). *)
let mat_bin (mat_array : (bool * (float array array)) array) =
  let bin i (complex,m) =
    if m = [||]
      then ""
      else
        (* If complex, then there are actually row/2 rows of complex numbers,
          otherwise there are row rows of real numbers *)
        let row = Array.length m in
        let actual_row =
          if complex
            then row/2
            else row
        in
        let col = Array.length m.(0) in
        let t = matrix_to_array m in
        let subh = obj_subheader "MAT" (String.make 1 Char.chr (65+i)) (16+12*row*col) in
        let pre_data =
          (String.make 8 '\000')
          ^(String.init 2 (fun j -> if j = 0 then Char.chr (actual_row/256) else Char.chr (actual_row mod 256)))
          ^(String.init 2 (fun j -> if j = 0 then Char.chr (col/256) else Char.chr (col mod 256)))
          ^(String.make 4 '\000')
        in
        let data_l =
          List.init (row*col)
            (fun i -> float_to_bin t.(i)
              (complex && i < actual_row*col && t.(i+actual_row*col) <> 0.)) in
        subh^pre_data^(String.concat "" data_l)
  in
  let data_l = Array.to_list (Array.mapi bin mat_array) in
  (String.concat "" data_l);;




(*********************************************************************************************)

(* General g1m writer function *)

(* Generates a G1M file named file_name, that contains all the objects
  in the project_content p *)
let g1m_writer (p : project_content) (file_name : string) : unit =
  let prog_data = prog_bin p.prog in
  let pict_data = pict_bin p.pict in
  let capt_data = capt_bin p.capt in
  let str_data = str_bin p.str in
  let list_data = list_bin p.list in
  let mat_data = mat_bin p.mat in
  let all_data = [prog_data; pict_data; capt_data; str_data; list_data; mat_data] in
  let data = String.concat "" all_data in
  let total_length = 32 + String.length data in
  let head = init_header "g1m" total_length (number_of_elements p) in
  let file_content = head^data in
  write_file file_name file_content;;