(* Temporary main function *)
(* Launches all the programs in order *)

(* To be replaced by a makefile *)

#use "topfind"
#require "sdl2"
#require "sdl2_ttf"
#require "unix"

(** Initialization **)
#use "src/initialization/graphics/graphic_parameters.ml"
#use "src/initialization/graphics/events.ml"
#use "src/initialization/key_check.ml"
#use "src/initialization/file_readers/file_reader.ml"
#use "src/initialization/file_readers/bmp_reader.ml"

#use "src/initialization/types/complex.ml"
#use "src/initialization/types/arithmetic_types.ml"
#use "src/initialization/types/graphic_types.ml"
#use "src/initialization/types/basic_type.ml"
#use "src/initialization/encodings/locate_format.ml"
#use "src/initialization/variables.ml"
#use "src/initialization/types/project_type.ml"

#use "src/initialization/encodings/basic_encoding.ml"
#use "src/initialization/encodings/command_display.ml"

#use "src/g1m_read_write/g1m_reader.ml"

(** Compilation **)
(* Arithmetic *)
#use "src/arithmetic/definitions/arithmetic_def.ml"
#use "src/arithmetic/definitions/entity_functions.ml"
#use "src/arithmetic/definitions/string_func.ml"
#use "src/arithmetic/arithmetic_lexing.ml"
#use "src/arithmetic/arithmetic_evaluation.ml"

#use "src/basic_compilation/initialization/data_structures.ml"
#use "src/basic_compilation/initialization/compilation_error.ml"
#use "src/basic_compilation/compile_aux.ml"
#use "src/basic_compilation/process_commands.ml"
#use "src/basic_compilation/basic_compile.ml"

(** Emulation **)
#use "src/initialization/graphics/colors.ml"
#use "src/initialization/graphics/graphics_lib.ml"
#use "src/basic_execution/initialization/float_repr.ml"
#use "src/basic_execution/initialization/timer.ml"
#use "src/basic_execution/initialization/runtime_error.ml"
#use "src/basic_execution/graphic_functions/general.ml"
#use "src/basic_execution/graphic_functions/text_mode.ml"
#use "src/basic_execution/graphic_functions/graphic_mode.ml"
#use "src/basic_execution/graphic_functions/wait_press.ml"
#use "src/basic_execution/graphic_functions/menu.ml"
#use "src/basic_execution/auxiliary/run_aux.ml"
#use "src/basic_execution/auxiliary/qmark.ml"
#use "src/basic_execution/auxiliary/graphics/fline.ml"
#use "src/basic_execution/auxiliary/graphics/graphic_commands_aux.ml"
#use "src/basic_execution/auxiliary/graphics/execute_graphic_commands.ml"
#use "src/basic_execution/basic_run.ml"

(* Tests *)
(* #use "tests/tests_run.ml" *)

(* Main menu *)
#use "src/main_menu/main_menu.ml"

(* Picture generation *)
(*
#use "src/picture_editor/picture_edit.ml"
#use "src/picture_editor/picture_creator.ml"
#use "src/g1m_read_write/g1m_writer.ml"
*)

(*

Games tested:
  - TIMELESS, PAC-MAN, RUN & JUMP, SUPER RUN & JUMP, ACE COMBAT, TIMELESS REMIX, AIRWOLF, SNIPER
    ICESLIDER, ARKENSTONE, CLONELAB, ROBSCAPE


*** To do:

Menus:
  - Draw a calculator and link it to Getkey (by clicking on keys)
    (key pressed on the keyboard highlight keys of the calculator interface)
  - Implement the menu:
    -> Options menu: changing keybinds
    -> Pause option
    ...

In the distant future:
  - Complete the "Done" print: a lot of functions print "Done" on the tscreen, so we may finish with "Done Done Done Done ..."
  - Treat the EOL/DISP in Strings vs Locate (EOL shows in Strings, syntax error in Locate. Disp shows in the Locate display, syntax error in String.)
  - Code the "ABC"?->X without skipping a line
  - Implement the fraction display (5 FRACSIGN 3 displays as a pair of int separated by FRACSIGN)
  - Implement the menu in the QMark entry menu, to access all functions
  - Implement the Disp command on lists and matrices
  - Improve the side by side interface, by allowing changing the conversion window when clicking
*)