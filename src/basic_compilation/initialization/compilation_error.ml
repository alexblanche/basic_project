(* Compilation error handling *)

exception Compilation_error of string list * int * string;;

(* Raises the exception *)
let fail (t : string list) (i : int) (error_message : string) =
  raise (Compilation_error (t, i, error_message));;

(* Skip to the next EOL or DISP *)
let rec extract_line (lexlist : string list) : string list * string list =
  let rec aux (acc : string list) (l : string list) =
    match l with
      | a::t ->
        if a = "EOL" || a = "COLON" || a = "DISP"
          then (a::acc, t)
          else aux (a::acc) t
      | [] -> (acc, [])
  in
  aux [] lexlist;;

(* Prints one line of the program *)
let print_lexline (line : string list) (eol : string) : unit =
  List.iteri
    (fun i s ->
      if i mod 20 = 0 then
        print_endline (String.escaped s)
      else
        (print_string (String.escaped s);
        print_char ' '))
    line;
  print_endline eol;;

let skip_until (lexlist : string list) (i : int) =
  let rec aux prev1 prev2 prev3 prev4 prev5 l k =
    match l with
      | [] -> (prev1, prev2, prev3, prev4, prev5, [])
      | _ ->
        let (line, t') = extract_line l in
        let line_len = List.length line in
        if k + line_len > i
          then (prev1, prev2, prev3, prev4, prev5, l)
          else aux prev2 prev3 prev4 prev5 line t' (k+line_len)
      
  in
  aux [] [] [] [] [] lexlist 0;;


(* Prints 5 lines before index i and 5 lines after *)
let print_around (lexlist : string list) (i : int) : unit =
  let rec print_five (l : string list) (k : int) =
    if k < 5 then
      (let (line, t) = extract_line l in
      match line with
        | eol :: sl ->
          (print_lexline (List.rev sl) eol;
          print_five t (k+1))
        | [] -> ())
  in

  let (prev1, prev2, prev3, prev4, prev5, t) = skip_until lexlist i in
  List.iter
    (fun s ->
      match s with
        | eol::sl -> print_lexline (List.rev sl) eol
        | [] -> ())
    [prev1; prev2; prev3; prev4; prev5];
  print_endline ">>> Error in this line:";
  let (line, t') = extract_line t in
  match line with
    | eol :: sl ->
      (print_lexline (List.rev sl) eol;
      print_endline "<<<";
      print_five t' 0)
    | [] -> ();;

(* Prints the error message and the 5 lines before and after the line where the compilation error occured *)
let print_error_info (lexlist : string list) (name : string) (len_t : int) (error_message : string) : unit =
  let len_lex = List.length lexlist in
  print_endline "---------------------------------------";
  print_endline ("Compilation error in program \""^name^"\"");
  print_endline error_message;
  print_newline ();
  print_around lexlist (len_lex - len_t);
  print_endline "---------------------------------------";;