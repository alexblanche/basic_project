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
#use "basic_running/graphic.ml"

#use "basic_running/float_repr.ml"
#use "basic_running/basic_run.ml"

(* Picture generation *)
(* #use "picture_editor/picture_creator.ml"
#use "basic_parsing/g1m_writer.ml" *)
