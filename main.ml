(* Temporary main function *)
(* Used to launch all the programs in order *)

(* To be replaced by a makefile *)

#use "topfind"
#require "sdl2"
#require "sdl2_ttf"
#require "unix"

(** Initialization **)
#use "initialization/graphics/graphic_parameters.ml"
#use "initialization/graphics/events.ml"
#use "initialization/key_check.ml"
#use "initialization/file_readers/file_reader.ml"
#use "initialization/file_readers/bmp_reader.ml"

#use "initialization/types/complex.ml"
#use "initialization/types/arithmetic_types.ml"
#use "initialization/types/graphic_types.ml"
#use "initialization/types/basic_type.ml"
#use "initialization/encodings/locate_format.ml"
#use "initialization/types/project_type.ml"

#use "initialization/encodings/basic_encoding.ml"
#use "initialization/encodings/command_display.ml"

#use "g1m_read_write/g1m_reader.ml"

(** Compilation **)
(* Arithmetic *)
#use "arithmetic/definitions/arithmetic_def.ml"
#use "arithmetic/definitions/entity_functions.ml"
#use "arithmetic/definitions/string_func.ml"
#use "arithmetic/arithmetic_lexing.ml"
#use "arithmetic/arithmetic_evaluation.ml"

#use "basic_compilation/initialization/data_structures.ml"
#use "basic_compilation/initialization/compilation_error.ml"
#use "basic_compilation/compile_aux.ml"
#use "basic_compilation/process_commands.ml"
#use "basic_compilation/basic_compile.ml"

(** Emulation **)
#use "initialization/graphics/graphics_lib.ml"
#use "basic_execution/initialization/float_repr.ml"
#use "basic_execution/graphic_functions/general.ml"
#use "basic_execution/graphic_functions/text_mode.ml"
#use "basic_execution/graphic_functions/graphic_mode.ml"
#use "basic_execution/graphic_functions/wait_press.ml"
#use "basic_execution/auxiliary/run_aux.ml"
#use "basic_execution/auxiliary/qmark.ml"
#use "basic_execution/auxiliary/graphic_commands_aux.ml"
#use "basic_execution/auxiliary/execute_graphic_commands.ml"
#use "basic_execution/basic_run.ml"

(* Tests *)
#use "tests/tests_run.ml"

(* Launches the program in the given G1M/G2M file *)
let play (file_name : string) (entry_point : string) : unit =
  let s = file_to_string file_name in
  let p = g1m_reader s in
  let code = compile p.prog in
  run p code entry_point;;


(* Picture generation *)
(*
#use "picture_editor/picture_edit.ml"
#use "picture_editor/picture_creator.ml"
#use "g1m_read_write/g1m_writer.ml"
*)

(*
Games that work:
  - TIMELESS, PAC-MAN, RUN & JUMP, SUPER RUN & JUMP
Next steps (by order of difficulty):
  - ACE COMBAT, TIMELESS REMIX, CLONELAB, AIRWOLF

To do next:
  - Keep implementing the "main" functions
  
  Menus:
  - Implement the entry point menu
  - Draw a calculator and link it to Getkey (by clicking on keys)
    (key pressed on the keyboard highlight keys of the calculator interface)

In the distant future:
  - Redo the "Done" print: a lot of functions print "Done" on the tscreen, so we may finish with "Done Done Done Done ..."
  - Treat the EOL/DISP in Strings vs Locate (EOL shows in Strings, syntax error in Locate. Disp shows in the Locate display, syntax error in String.)
  - Code the "ABC"?->X without skipping a line
  - Complete the list of encodings, with the Catalog
  - Implement the fraction display (5 FRACSIGN 3 displays as a pair of int separated by FRACSIGN)
  - Implement the menu in the QMark entry menu, to access all functions
  - Improve the side by side interface, by allowing changing the conversion window when clicking
*)