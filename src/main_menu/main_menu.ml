(* Main "PROGRAM" menu *)

(* Draws the title of the menu *)
let draw_prog_menu_title (ren : Sdlrender.t) : unit =
  
  (* The text is shifted by 1 pixel horizontally and 3 pixels vertically for Locate 2,1 *)
  (* margin_v := !margin_v + 3 * !size;
  margin_h := !margin_h + !size; *)
  
  let title = List.rev ["P"; "R"; "O"; "G"; "R"; "A"; "M"; "S"] in
  let acc = ref [] in
  fast_locate_aux false 6 0 acc title 13;
  Sdlrender.fill_rects ren (Array.of_list (horizontal_rect 0 127 11 :: !acc));;

  (* The margins are shifted back to their original position *)
  (* margin_v := !margin_v - 3 * !size;
  margin_h := !margin_h - !size;; *)

(* Draws the entries of the menu *)
(* The highest entry has index 'highest', and pindex is the current one. *)
let draw_prog_menu_entries (ren : Sdlrender.t) (entries : info array)
  (pindex : int) (highest : int) : unit =

  (* White rectangle over the entries *)
  set_color ren colors.background;
  let white_r =
    Sdlrect.make
      ~pos:(!margin_h + !size, !margin_v + 13 * !size)
      ~dims:(!size * 127, !size * 51)
  in
  Sdlrender.fill_rect ren white_r;
  set_color ren colors.pixels;

  let n = Array.length entries in
  let acc = ref [] in

  for i = 0 to min 5 (n-1 - highest) do

    let (prog_name, prog_size, _, _) = entries.(highest + i) in
    let name_repr = str_to_rev_symblist_full prog_name in

    let size_repr =
      let l = ref [] in
      let n = ref prog_size in
      while !n > 0 do
        l := (string_of_int (!n mod 10)) :: !l;
        n := !n / 10
      done;
      List.rev !l
    in

    if highest + i = pindex then
      ((* Black background *)
      fill_rect ren
        (!margin_h + 3 * !size) (!margin_v + (i+2) * 8 * !size)
        (125 * !size) (8 * !size);
      let white_acc = ref [] in
      (* Program name *)
      fast_locate_aux false 1 (i+2) white_acc name_repr (min 8 (List.length name_repr));
      (* ":" *)
      fast_locate_aux false 12 (i+2) white_acc [":"] 12;
      (* program size *)
      fast_locate_aux false 14 (i+2) white_acc size_repr 19;

      set_color ren colors.background;
      Sdlrender.fill_rects ren (Array.of_list !white_acc);
      set_color ren colors.pixels)
    else
      (* Program name *)
      (fast_locate_aux false 1 (i+2) acc name_repr (min 8 (List.length name_repr));
      (* ":" *)
      fast_locate_aux false 12 (i+2) acc [":"] 12;
      (* program size *)
      fast_locate_aux false 14 (i+2) acc size_repr 19)
  done;

  Sdlrender.fill_rects ren (Array.of_list !acc);;
  

(* Draws the initial menu *)
(* entries = Array.of_list content.prog, of type (string list * int * int * int) array *)
let draw_prog_menu (ren : Sdlrender.t) (entries : info array) (pindex : int) (highest : int) : unit =

  clear_graph ren;

  draw_frame ren;
  draw_prog_menu_title ren;
  draw_prog_menu_entries ren entries pindex highest;

  refresh ren;;



(* Runs the main menu *)
let main_menu ((code, proglist) : basic_code) (proj : project_content) (content : content)
  (verbose : bool) : unit =

  (* Creation of the window and the renderer *)
  let (win, ren) = open_graphic () in
  let progt = Array.of_list proglist in
  let entries = Array.of_list content.prog in
  (* Sorting of the two arrays *)
  let _ =
    let comp_str s1 s2 =
      if s1 > s2 then
        1
      else if s1 = s2 then
        0
      else
        -1
    in
    Array.sort (fun (s1,_) (s2,_) -> comp_str s1 s2) progt;
    Array.sort (fun (s1,_,_,_) (s2,_,_,_) -> comp_str s1 s2) entries
  in
  
  
  (* Creation of the key check domain *)
  exit_key_check := false;
  let key_check_domain = Domain.spawn launch_key_check in

  (* Initialization of the parameters *)
  let (p : parameters) = new_param proj in

  p.bgscreen <- bgscreen;
  p.gscreen <- gscreen;
  p.bgpict <- -1;
  set_real_var p.var xmin_index 1.;
  set_real_var p.var xmax_index 127.;
  set_real_var p.var xscl_index 0.;
  set_real_var p.var ymin_index 1.;
  set_real_var p.var ymax_index 63.;
  set_real_var p.var yscl_index 0.;
  set_real_var p.var xdot_index 1.;
  p.axeson <- false;

  set_color ren colors.pixels;
  getkey := 0;
  key_pressed := Sdlkeycode.Unknown;
  

  let n = Array.length entries in


  let rec prog_select_loop (pindex : int) (highest : int) : int =
    (* Window closed *)
    if !exit_key_check then
      0
    else

    (* Resizing *)
    let _ =
      if !parameters_updated then
        (parameters_updated := false;
        clear_graph ren;
        draw_frame ren;
        draw_prog_menu ren entries pindex highest;
        refresh ren)
    in

    (* Program selection key handling *)
    match !key_pressed with
      | Up ->
        (let new_pindex, new_highest =
          if pindex <> 0 then
            (pindex - 1, min (pindex - 1) highest)
          else
            (n-1, max (n-6) 0)
        in
        draw_prog_menu_entries ren entries new_pindex new_highest;
        refresh ren;
        wait_release ren false;
        prog_select_loop new_pindex new_highest)

      | Down ->
        (let new_pindex, new_highest =
          if pindex <> n-1 then
            (pindex + 1,
              if highest = pindex - 5
                then highest + 1
                else highest)
          else
            (0, 0)
        in
        draw_prog_menu_entries ren entries new_pindex new_highest;
        refresh ren;
        wait_release ren false;
        prog_select_loop new_pindex new_highest)

      | Return
      | KP_Enter -> pindex

      | _ -> prog_select_loop pindex highest
  in

  let rec menu_loop () : unit =
    wait_release ren false;

    (* Display of the program menu *)
    draw_prog_menu ren entries 0 0;
    
    (* Choice of the program to run *)
    let chosen_pindex = prog_select_loop 0 0 in
    
    if not !exit_key_check then
      (let entry_index = snd (progt.(chosen_pindex)) in
      
      (* Running the chosen program *)
      (try
        run_program win ren p proj (code, proglist) entry_index verbose
      with
        | Runtime_interruption
        | Window_Closed -> print_endline "--- Runtime interruption ---"
        | Invalid_argument s
        | Failure s -> (print_string "Runtime failure: "; print_endline s)
        | Not_found -> print_endline "Runtime error: Not_found");
      menu_loop ())
  in
  
  (* Running the menu loop *)
  (try
    menu_loop ()
  with
    | _ -> ());
  
  (* Exitting *)
  exit_key_check := true;
  Domain.join key_check_domain;
  close_graph win;
  sdl_quit ();;


(* Main function: launches the emulator *)
let main (file_name : string) (verbose : bool) (compile_display_all : bool)
  (ignore_compilation_errors : bool) : unit =

  let s = file_to_string file_name in
  let p = g1m_reader s verbose in
  let c = get_content s in
  let codeprogl = compile p.prog verbose compile_display_all ignore_compilation_errors in
  main_menu codeprogl p c verbose;;


(** Functions that launch the programs with different parameters **)

(* Default running function *)
let run (file_name : string) : unit =
  main file_name false false true;;

(* Running function with compilation errors and warnings displayed *)
let run_verbose (file_name : string) : unit =
  main file_name true false true;;

(* Debugging functions *)

(* Strict compilation: for debugging purposes, no command is ignored *)
let run_strict (file_name : string) : unit =
  main file_name false false false;;

(* Returns the compiled code and the list of entry indices for each program *)
let get_compiled_code (file_name : string) : basic_code =
  let s = file_to_string file_name in
  let p = g1m_reader s true in
  compile p.prog true false true;;

(* Same, but displays each line of the code (useful to spot an infinite loop during compile time) *)
let get_compiled_code_verbose (file_name : string) : basic_code =
  let s = file_to_string file_name in
  let p = g1m_reader s true in
  compile p.prog true true true;;