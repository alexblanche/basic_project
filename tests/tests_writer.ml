(* Tests for G1M file generation *)

#use "basic_parsing/g1m_writer.ml"
#use "picture_editor/picture_drawer.ml"

(** Examples of objects **)

(* Examples of programs *)
let prog_a =
  ("PROGA",
  ["IF"; "A"; "EQUAL"; "0"; ":"; "THEN"; "1"; "EOL"; "ELSE"; "0"; "EOL"; "IFEND"; "EOL"]
  );;
let prog_b =
  ("PROG~B",
  ["FLINE"; "1"; "0"; ","; "2"; "0"; ","; "1"; "0"; "0"; ","; "4"; "5"; "EOL"]
  );;

(* Examples of strings *)
let str1 = "Hello.";;
let str2 = "Bye!";;
let str3 = "This string is longer. Not that long but like a little longer.";;

(* Examples of lists *)
let list1 = (true, [| 12.345678 ; 123.456789 ; 123e65 ; 5. ; 4. ; 3. ; 2. ; 1. |]);;
let list2 = (true, [| -0.05 ; -1e-10 ; 0.001 |]);;

(* Examples of matrices *)
let mat1 = (true, [| [| 1. ; 2. ; 3. ; 4. ; 5. |] ; [| 1. ; 2. ; 3. ; 4. ; 5. |] ; [| 1. ; 2. ; 3. ; 4. ; 5. |] ; [| 1. ; 2. ; 3. ; 4. ; 5. |] |]);;
let mat2 = (true, [| [| 1.; 11.; 111.; 1111.; 11111.; 111111.; 1111111.; 11111111. |] |]);;

(* Pictures and captures are created via the interface when the file is created *)


(** Examples of projects **)

let proj_all =
  {
    prog = [prog_a; prog_b];
    list =
      (let list_array = Array.make 26 (true, [||]) in
      (* Lists 9, 12 *)
      list_array.(8) <- list1;
      list_array.(11) <- list2;
      list_array);
    mat =
      (let mat_array = Array.make 26 (true, [||]) in
      (* Matrices E, F *)
      mat_array.(4) <- mat1;
      mat_array.(5) <- mat2;
      mat_array);
    pict =
      (let im1 = interface true in
      let im2 = interface true in
      let pict_array = Array.make 20 (0,[||]) in
      (* Pictures 18, 19 *)
      pict_array.(17) <- (2048, im1);
      pict_array.(18) <- (2048, im2);
      pict_array);
    capt =
      (let im1 = interface true in
      let im2 = interface true in
      let capt_array = Array.make 20 [||] in
      (* Captures 18, 19 *)
      capt_array.(17) <- im1;
      capt_array.(18) <- im2;
      capt_array);
    (* Strings 7, 10, 12 *)
    str = 
      (let str_array = Array.make 20 "" in
      str_array.(6) <- str1;
      str_array.(9) <- str2;
      str_array.(11) <- str3;
      str_array)
  };;

let proj_prog =
  {
    prog = [prog_a; prog_b];
    list = Array.make 26 (true, [||]);
    mat = Array.make 26 (true, [||]);
    pict = Array.make 20 (0, [||]);
    capt = Array.make 20 [||];
    str = [||]
  };;

let proj_cplm =
  {
    prog = [];
    list =
      (let list_array = Array.make 26 (true, [||]) in
      list_array.(0) <-
        (true, [| 1.; 2.; 3.; 4.; 5.; 6.; 7.; 0.; 0.; 0.; 8.; -.2.; 0.; 0. |]);
      list_array
      );
    mat =
      (let mat_array = Array.make 26 (true, [||]) in
      mat_array.(0) <-
        (true, [| [| 1.; 2. |]; [| 3.; 4. |]; [| 5.; 6. |]; [| 0.; 0. |]; [| 1.; 0. |]; [| 0.; 2. |] |]);
      mat_array
      );
    pict = Array.make 20 (0, [||]);
    capt = Array.make 20 [||];
    str = [||]
  };;

(** File generation **)

let folder = "/mnt/c/users/blanc/desktop/ocaml_tests/basic_project/data/";;

let gen_all () = g1m_writer proj_all (folder^"genpall.g1m");;

let gen_prog () = g1m_writer proj_prog (folder^"genpprog.g1m");;

let gen_cplm () = g1m_writer proj_cplm (folder^"genpcplm.g1m");;