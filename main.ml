(* Temporary main function *)
(* Used to launch all the programs in order *)

#use "topfind"
#require "sdl2"
#require "sdl2_ttf"

(* Initialization *)
#use "initialization/graphics/graphic_parameters.ml"
#use "initialization/graphics/events.ml"
#use "initialization/key_check.ml"
#use "initialization/file_readers/file_reader.ml"
#use "initialization/file_readers/bmp_reader.ml"
#use "initialization/types/complex.ml"
#use "initialization/types/basic_type.ml"
#use "initialization/types/project_type.ml"
#use "initialization/basic_encoding.ml"
#use "initialization/command_display.ml"

#use "g1m_read_write/g1m_reader.ml"

(* Compilation *)
#use "arithmetic/arithmetic_def.ml"
#use "arithmetic/arithmetic_lexing.ml"
#use "arithmetic/arithmetic_parsing.ml"

#use "basic_parsing/basic_compilation.ml"

(* Emulation *)
#use "initialization/graphics/graphics_lib.ml"

#use "basic_running/float_repr.ml"
#use "basic_running/locate_format.ml"
#use "basic_running/basic_graphics.ml"
#use "basic_running/run_aux.ml"
#use "basic_running/qmark.ml"
#use "basic_running/basic_run.ml"

#use "tests/tests_run.ml"

(* Launches the program in the given G1M/G2M file *)
let play (file_name : string) : unit =
  let s = file_to_string file_name in
  let p = g1m_reader s in
  let code = compile p.prog in
  run p code;;


(* Picture generation *)
(*
#use "picture_editor/picture_edit.ml"
#use "picture_editor/picture_creator.ml"
#use "g1m_read_write/g1m_writer.ml"
*)


(* To do immediately:
  - Integrate list/matrix arithmetic in arithmetic_lexing, parsing and basic_compilation (cf comment in airthmetic_def)
  
To do next:
  - Code graphic functions (Cls, View-Window, F-line, all DrawStat-related functions...)
  - Slow down the execution with Unix.sleepf (tests needed for text and graphic display)
  - Draw a calculator and code the execution of Getkey (by clicking or pressing keys)
    (Idea: key pressed on the keyboard highlight keys of the calculator interface)
  - Recode the operators for complex numbers (espacially '=')
  - Implement List Ans and Mat Ans (analog to Ans for list_expr and mat_expr)

In the distant future:
  - Redo the "Done" print: a lot of functions print "Done" on the tscreen, so we may finish with "Done Done Done Done ..."
  - Treat the EOL/DISP in Strings vs Locate (EOL shows in Strings, syntax error in Locate. Disp shows in the Locate display, syntax error in String.)
  - Code the "ABC"?->X without skipping a line
  - Complete the list of encodings, with the Catalog
  - Implement the fraction display (5 FRACSIGN 3 displays as a pair of int separated by FRACSIGN)
  - Implement as many functions as possible (from the to do list)
  - Implement the menu in the QMark entry menu, to access all functions
  - Improve the side by side interface, by allowing changing the conversion window when clicking
*)