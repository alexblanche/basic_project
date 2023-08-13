(* G1M/G2M file writer *)

(* TO DO: Subheader needs testing *)

(* Creates the file file_name and writes the string s in it *)
let write_file (file_name : string) (s : string) : unit =
  let oc = open_out file_name in
  output_string oc s;
  close_out oc;;

(*******************************************************************)

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

(* Test: generates a g1m file with name file_name, containing
  one picture (called PICT1) based on boolean matrix m *)
  (* The picture is uncompressed. *)
let write_pict (m : bool array array) (file_name : string) =
  let data = bool_mat_to_pict_bin m 1024 in
  let subh = obj_subheader "PICTURE" "1" 2048 in
  let length = (String.length subh) + 2048 + 32 in
  let head = init_header "g1m" length 1 in
  let padding = String.make 1024 '\000' in
  let file_content = head^subh^data^padding in
  write_file file_name file_content;;

(* Test: generates a g1m file with name file_name, containing
  all the pictures in m_list (called PICT1,2,...)
  Each element of m_list is a pair (m,nb_bytes), where m is a
  boolean matrix and nb_bytes is the number of bytes read
  (for compressed pictures that have nb_bytes < 1024)
  For uncompressed pictures: nb_bytes = 2048 *)
let write_picts (m_list : (bool array array * int) list) (file_name : string) : unit =
  let indices = [|1;10;11;12;13;14;15;16;17;18;19;2;20;3;4;5;6;7;8;9|] in
  let bin i (m,nb_bytes) =
    let subh = obj_subheader "PICTURE" (string_of_int indices.(i)) (min nb_bytes 2048) in
    let padding =
      String.make (if nb_bytes = 2048 then 1024 else (4-(nb_bytes mod 4)) mod 4) '\000'
    in
    subh^(bool_mat_to_pict_bin m (min nb_bytes 1024))^padding
  in
  let data_l = List.mapi bin m_list in
  (* length = sum of the lengths of the strings representing each object
      + 32 (size of the initial header) *)
  let length =
    (List.fold_left (fun sum s -> sum + String.length s) 0 data_l) + 32
  in
  let head = init_header "g1m" length (List.length m_list) in
  let file_content = head^(String.concat "" data_l) in
  write_file file_name file_content;;

(* Test: generates a g1m file with name file_name, containing
  one capture (called CAPT1) based on boolean matrix m *)
let write_capt (m : bool array array) (file_name : string) : unit =
  let data = bool_mat_to_capt_bin m in
  let subh = obj_subheader "CAPT" "1" (4 + 1024) in
  let length = (String.length subh) + 4 + 1024 + 32 in
  let head = init_header "g1m" length 1 in
  let file_content = head^subh^"\000\128\000\064"^data in
  write_file file_name file_content;;

(* Both functions successfully write pictures/captures that are
  correctly recognized by the calculator, and they can display pixels
  on the normally inaccessible left and top lines *)

(* Test: generates a g1m file with name file_name, containing
  all the strings in the list str_list (called STR1, STR2, ...) *)
let write_str (str_list : string list) (file_name : string) : unit =
  let bin i str =
    let len = String.length str in
    let padding_size = 4 - (len mod 4) in
    let subh = obj_subheader "STRING" (string_of_int (i+1)) (len + padding_size) in
    subh^str^(String.make padding_size '\000')
  in
  let data_l = List.mapi bin str_list in
  (* length = sum of the lengths of the strings representing each object
      + 32 (size of the initial header) *)
  let length =
    (List.fold_left (fun sum s -> sum + String.length s) 0 data_l) + 32 in
  let head = init_header "g1m" length (List.length str_list) in
  let file_content = head^(String.concat "" data_l) in
  write_file file_name file_content;;


(*********************************************************************************************)

(** First attempt at generating lists/matrices **)
(* I struggle to understand the file structure of lists...
  Are the unused bytes unimportant? Can we put anything in them, or are they all control bytes? *)

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
let float_to_bin (x : float) : string =
  let pow = Float.floor (Float.log10 (Float.abs x)) in
  (* Info half-byte value *)
  let info =
    8*0
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

(* Test: generates a g1m file with name file_name, containing
  the list t, named "LIST 1" *)
let write_list (t : float array) (file_name : string) =
  let length = Array.length t in
  let subh = obj_subheader "LIST" "1" (16+12*length) in
  let pre_data =
    (String.make 8 '\000')
    ^(String.init 1 (fun _ -> Char.chr (length/256)))
    ^(String.init 1 (fun _ -> Char.chr (length mod 256)))
    ^(String.make 6 '\000')
  in
  let data_l = List.init length (fun i -> float_to_bin t.(i)) in
  let data = String.concat "" data_l in
  let head = init_header "g1m" (32 + String.length subh + 16 + 12*length) 1 in
  let file_content = head^subh^pre_data^data in
  write_file file_name file_content;;