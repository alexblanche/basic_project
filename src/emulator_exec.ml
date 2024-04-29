(* Emulator main function *)

let () =
  match Sys.argv with
    | [| _; file_name |]
    | [| _; "--verbose"; file_name |] ->
      (if Sys.file_exists file_name then
        if Array.length Sys.argv = 2 then
          run file_name
        else
          run_verbose file_name
      else
        (sdl_quit ();
        print_endline ("File "^file_name^" not found"))
      )
    | _ ->
      (sdl_quit ();
      print_endline "Error: wrong arguments";
      let exec_name = Filename.basename (Sys.executable_name) in
      print_endline ("Usage: ./"^exec_name^" file_name.ml or ./"^exec_name^" --verbose file_name.ml"))
;;