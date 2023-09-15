(* Temporary main function *)
(* Used to launch all the programs in order *)

#use "topfind"
#require "sdl2"
#require "sdl2_ttf"

(* Initialization *)
#use "initialization/graphics/graphic_parameters.ml"
#use "initialization/graphics/events.ml"
#use "initialization/file_readers/file_reader.ml"
#use "initialization/file_readers/bmp_reader.ml"
#use "initialization/types/complex.ml"
#use "initialization/types/basic_type.ml"
#use "initialization/types/project_type.ml"
#use "initialization/basic_encoding.ml"
#use "initialization/command_display.ml"

#use "g1m_read_write/g1m_reader.ml"

(* Compilation *)
#use "basic_running/getkey.ml"
#use "arithmetic/arithmetic_def.ml"
#use "arithmetic/arithmetic_lexing.ml"
#use "arithmetic/arithmetic_parsing.ml"

#use "basic_parsing/basic_compilation.ml"

(* Emulation *)
#use "initialization/graphics/graphics_lib.ml"

#use "basic_running/float_repr.ml"
#use "basic_running/locate_format.ml"
#use "basic_running/basic_graphics.ml"
(* Not yet adapted to SDL2 *)
#use "basic_running/basic_run.ml"

#use "tests/tests_run.ml"


(* Picture generation *)
(*
#use "picture_editor/picture_edit.ml"
#use "picture_editor/picture_creator.ml"
#use "g1m_read_write/g1m_writer.ml"
*)


(* To do immediately:
  - Encapsulate the type complex (in Value) into another type also containing whole lists and matrices
  - Integrate list/matrix arithmetic in arithmetic_lexing, parsing and basic_compilation
  
To do next:
  - Complete the implementation of QMark (with functions, commands and operators)
  - Slow down the execution with Unix.sleepf (tests needed for text and graphic display)
  - Draw a calculator and code the execution of Getkey (by clicking or pressing keys)
    (Idea: key pressed on the keyboard highlight keys of the calculator interface)
  - Code "=>"
  - Integrate ":" with "EOL" (via a function is_eol)
  - Recode the operators for complex numbers (espacially '=')
  (At this point, the original Timeless might run.)
  - Code graphic functions (Cls, View-Window, F-line, all DrawStat-related functions...)

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