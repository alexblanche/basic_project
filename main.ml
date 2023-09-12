(* Temporary main function *)
(* Used to launch all the programs in order *)

(* Emulation *)
#use "basic_parsing/file_reader.ml"
#use "picture_editor/bmp_reader.ml"
#use "basic_parsing/arithmetic/complex.ml"
#use "basic_parsing/basic_type.ml"
#use "basic_parsing/project_type.ml"
#use "basic_parsing/basic_encoding.ml"
#use "basic_parsing/command_display"
#use "basic_parsing/g1m_reader.ml"

#use "basic_parsing/arithmetic/arithmetic_def.ml"
#use "basic_parsing/arithmetic/arithmetic_lexing.ml"
#use "basic_parsing/arithmetic/arithmetic_parsing.ml"
#use "basic_parsing/basic_compilation.ml"

#use "topfind"
#require "sdl2"
#require "sdl2_ttf"

#use "picture_editor/graphic_parameters.ml"
#use "picture_editor/graphics_lib.ml"
#use "basic_running/float_repr.ml"
#use "basic_running/locate_format.ml"
#use "basic_running/graphic.ml"

#use "basic_running/basic_run.ml"
#use "picture_editor/picture_edit.ml"

(* Picture generation *)
(*
#use "picture_editor/picture_edit.ml"
#use "picture_editor/picture_creator.ml"
#use "basic_parsing/g1m_writer.ml"
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
  - Use SDL2 with OCamlSDL2 to have more control over keyboard, windows, etc.

In the distant future:
  - Treat the EOL/DISP in Strings vs Locate (EOL shows in Strings, syntax error in Locate. Disp shows in the Locate display, syntax error in String.)
  - Code the "ABC"?->X without skipping a line
  - Complete the list of encodings, with the Catalog
  - Implement the fraction display (5 FRACSIGN 3 displays as a pair of int separated by FRACSIGN)
  - Implement as many functions as possible (from the to do list)
  - Implement the menu in the QMark entry menu, to access all functions
*)