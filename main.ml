(* Temporary main function *)
(* Used to launch all the programs in order *)

(* To do: redo the file placement *)

(* Emulation *)
#use "basic_parsing/file_reader.ml"
#use "picture_editor/bmp_reader.ml"
#use "basic_parsing/arithmetic/complex.ml"
#use "basic_parsing/basic_type.ml"
#use "basic_parsing/project_type.ml"
#use "basic_parsing/basic_encoding.ml"
#use "basic_parsing/g1m_reader.ml"

#use "basic_parsing/arithmetic/arithmetic_def.ml"
#use "basic_parsing/arithmetic/arithmetic_lexing.ml"
#use "basic_parsing/arithmetic/arithmetic_parsing.ml"
#use "basic_parsing/basic_compilation.ml"

#use "topfind"
#require "graphics"

#use "picture_editor/picture_drawer.ml"
#use "basic_running/float_repr.ml"
#use "basic_running/graphic.ml"

#use "basic_running/basic_run.ml"

(* Picture generation *)
(* #use "picture_editor/picture_creator.ml"
#use "basic_parsing/g1m_writer.ml" *)


(* To do immediately:
  - Recode function handling with lists of arguments
  - Recode the lexing of lists of arguments
  - Recode the parsing of functions
  - Encapsulate the type complex (in Value) into another type also containing whole lists and matrices
  - Integrate list/matrix arithmetic in arithmetic_lexing, parsing and basic_compilation
  
To do next:
  - Slow down the execution with Unix.sleepf (tests needed for text and graphic display)
  - Code the execution of QMark (number entry)
  - Draw a calculator and code the execution of Getkey (by clicking or pressing keys)
    (Idea: key pressed on the keyboard highlight keys of the calculator interface)
  - Code "Do LpWhile", "For To Next"
  - Recode the operators for complex numbers (espacially '=')
(At this point, the original Timeless might run.)
  - Code graphic functions (Cls, View-Window, F-line, all DrawStat-related functions...)

In the distant future:
  - Complete the list of encodings, with the Catalog
  - Implement the fraction display (5 FRACSIGN 3 displays as a pair of int separated by FRACSIGN)
  - Implement as many functions as possible (from the to do list)
  *)