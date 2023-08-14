(* Type for project content *)

(* Content of a project (g1m file) *)
(* Each object (except programs) is stored at index index_Casio-1. *)

(* The name of the program is stored with the list of lexemes *)
type program = string * (string list)

type project_content =
  {
    prog : program list;

    (* List indices range from 1 to 26 *)
    (* The first parameter is true if the list contains real numbers,
      false if it contains complex numbers *)
    list : (bool * (float array)) array;

    (* Matrix indices range from A to Z *)
    (* The first parameter is true if the matrix contains real numbers,
      false if it contains complex numbers *)
    mat : (bool * (float array array)) array;

    (* Picture indices range from 1 to 20 *)
    (* The first parameter is the number of lines of the pictures:
      - uncompressed pictures: 2048
      - compressed pictures: <= 1024 *)
    pict : (int * (bool array array)) array;

    (* Capture indices range from 1 to 20 *)
    capt : (bool array array) array;

    (* String indices range from 1 to 20 *)
    str : string array
  }