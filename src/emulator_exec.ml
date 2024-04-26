(* Emulator main function *)

let () =
  match Sys.argv with
    | [| file_name |] -> run file_name
    | [| "--verbose"; file_name |] -> run_verbose file_name
    | _ ->
      (print_endline "Error: wrong arguments";
      print_endline "Usage: ./basic_emulator file_name.ml or ./basic_emulator --verbose file_name.ml")
;;