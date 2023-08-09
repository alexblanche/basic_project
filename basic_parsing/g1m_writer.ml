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
    if obj_type = "CAPT"
      then ((String.make 4 '\000')^"\128\000\064")
      else String.make 7 '\000'
  in

  (* Subheader string *)
  data_type ^ "\001" ^ folder ^ name_part ^ file_type ^ size ^ padding;;


  (*
  - 19 bytes: Data type ("PROGRAM", "LIST 1", "MAT A", "PICTURE 1", "CAPT 1", "STRING 1")
    + padding with \000
  - 1 byte: \001
  - 8 bytes: "system" for programs, "@REV2" for captures, "main" for alpha-mem types
    + padding with \000
  - 8 bytes: the name of the object (name of the program, 1LIST1 (1LIST2...), MAT_M, PICT1, CAPT1, STR1)
    + padding with \000
  - 1 byte: type of file
    (\001 Basic program, \004 string, \005 list, \006 matrix, \007 picture, \010 capture)
  - 4 bytes: size of the program
  - 4 bytes \000
  - 3 bytes: "\128\000\064" for captures, "\000\000\000" otherwise
  [Default starting point]
  (if the object is:
    - a program: 6 bytes \000, then beginning of the program)
    - a capture: beginning of the image data
    - a picture: 12 bytes \000, then beginning of the image data)
  - Padding: some bytes \000
  *)

(*******************************************************************)

(** Conversions to binary **)

(* Conversion from boolean matrix to binary encoding of a picture *)
(* The matrix is assumed to have size 64*128. *)
(* The calculator prints the bottom line as the top one
  (normally inaccessible). This function takes this into account
  (with the (64+62-i/16) mod 64) argument). *)
let bool_mat_to_pict_bin (m : bool array array) : string =
  let aux i =
    let res = ref 0 in
    for j = 0 to 7 do
      res := 2 * !res + (if m.((64+62-i/16) mod 64).(8*(i mod 16)+j) then 1 else 0)
    done;
    char_of_int (!res)
  in
  String.init 1024 aux;;

(* Conversion from boolean matrix to binary encoding a capture *)
(* The matrix is assumed to have size 64*128. *)
let bool_mat_to_capt_bin (m : bool array array) : string =
  let aux i =
    let res = ref 0 in
    for j = 0 to 7 do
      res := 2 * !res + (if m.(63-i/16).(8*(i mod 16)+j) then 1 else 0)
    done;
    char_of_int (!res)
  in
  String.init 1024 aux;;

(*******************************************************************)

(** File generation **)

(* Test: generates a g1m file with name file_name, containing
  one picture (called PICT1) based on boolean matrix m *)
let write_pict (m : bool array array) (file_name : string) =
  let data = bool_mat_to_pict_bin m in
  let subh = obj_subheader "PICTURE" "1" 2048 in
  let length = (String.length subh) + 12 + 2032 + 32 in
  let head = init_header "g1m" length 1 in
  let padding = String.make 1008 '\000' in
  let file_content = head^subh^(String.make 12 '\000')^data^padding in
  write_file file_name file_content;;

(* Test: generates a g1m file with name file_name, containing
  one capture (called CAPT1) based on boolean matrix m *)
  let write_capt (m : bool array array) (file_name : string) =
    let data = bool_mat_to_capt_bin m in
    let subh = obj_subheader "CAPT" "1" 1028 in
    let length = (String.length subh) + 1024 + 32 in
    let head = init_header "g1m" length 1 in
    let file_content = head^subh^data in
    write_file file_name file_content;;

(* Both functions successfully write pictures/captures that are
  correctly recognized by the calculator, and they can display pixels
  on the normally inaccessible left and top lines *)