OPT = @ocamlopt
FINDOPT = @ocamlfind opt
MKDIR = @mkdir
PRINT = @echo $<

SUFFIXES ?= .ml .o .cmx .cmi
.SUFFIXES: $(SUFFIXES) .

EXEC_EMULATOR = main
BUILDDIR = ./build

OBJS = $(BUILDDIR)/graphic_parameters.cmx \
	$(BUILDDIR)/events.cmx \
	$(BUILDDIR)/key_check.cmx \
	$(BUILDDIR)/file_reader.cmx \
	$(BUILDDIR)/bmp_reader.cmx \
	$(BUILDDIR)/complex_type.cmx \
	$(BUILDDIR)/arithmetic_types.cmx \
	$(BUILDDIR)/graphic_types.cmx \
	$(BUILDDIR)/basic_type.cmx \
	$(BUILDDIR)/locate_format.cmx \
	$(BUILDDIR)/variables.cmx \
	$(BUILDDIR)/project_type.cmx \
	$(BUILDDIR)/basic_encoding.cmx \
	$(BUILDDIR)/command_display.cmx \
	$(BUILDDIR)/g1m_reader.cmx \
	$(BUILDDIR)/arithmetic_def.cmx \
	$(BUILDDIR)/entity_functions.cmx \
	$(BUILDDIR)/string_func.cmx \
	$(BUILDDIR)/arithmetic_lexing.cmx \
	$(BUILDDIR)/arithmetic_evaluation.cmx \
	$(BUILDDIR)/data_structures.cmx \
	$(BUILDDIR)/compilation_error.cmx \
	$(BUILDDIR)/compile_aux.cmx \
	$(BUILDDIR)/process_commands.cmx \
	$(BUILDDIR)/basic_compile.cmx \
	$(BUILDDIR)/colors.cmx \
	$(BUILDDIR)/graphics_lib.cmx \
	$(BUILDDIR)/float_repr.cmx \
	$(BUILDDIR)/timer.cmx \
	$(BUILDDIR)/runtime_error.cmx \
	$(BUILDDIR)/general.cmx \
	$(BUILDDIR)/text_mode.cmx \
	$(BUILDDIR)/graphic_mode.cmx \
	$(BUILDDIR)/wait_press.cmx \
	$(BUILDDIR)/menu.cmx \
	$(BUILDDIR)/run_aux.cmx \
	$(BUILDDIR)/qmark.cmx \
	$(BUILDDIR)/fline.cmx \
	$(BUILDDIR)/graphic_commands_aux.cmx \
	$(BUILDDIR)/execute_graphic_commands.cmx \
	$(BUILDDIR)/basic_run.cmx \
	$(BUILDDIR)/main_menu.cmx \
	$(BUILDDIR)/emulator_exec.cmx

.PHONY: all clean

###########################################################
# Main targets
# To be added later: Picture editor interface

all: $(EXEC_EMULATOR)

clean:
	@rm -r $(BUILDDIR)
	@rm $(EXEC_EMULATOR)
	
$(BUILDDIR):
	$(MKDIR) $@

###########################################################
# Compilation instructions for each file

$(BUILDDIR)/graphic_parameters.cmx: src/initialization/graphics/graphic_parameters.ml | $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-package sdl2 \
		-package sdl2_ttf \
		-linkpkg


$(BUILDDIR)/events.cmx: src/initialization/graphics/events.ml \
	$(BUILDDIR)/graphic_parameters.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-package sdl2 \
		-linkpkg \
		-open Graphic_parameters


$(BUILDDIR)/key_check.cmx: src/initialization/key_check.ml \
	$(BUILDDIR)/graphic_parameters.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-package sdl2 \
		-linkpkg \
		-open Graphic_parameters


$(BUILDDIR)/file_reader.cmx: src/initialization/file_readers/file_reader.ml | $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@


$(BUILDDIR)/bmp_reader.cmx: src/initialization/file_readers/bmp_reader.ml | $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-package sdl2 \
		-linkpkg


$(BUILDDIR)/complex_type.cmx: src/initialization/types/complex_type.ml | $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@


$(BUILDDIR)/arithmetic_types.cmx: src/initialization/types/arithmetic_types.ml \
	$(BUILDDIR)/complex_type.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-open Complex_type


$(BUILDDIR)/graphic_types.cmx: src/initialization/types/graphic_types.ml \
	$(BUILDDIR)/arithmetic_types.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-open Arithmetic_types


$(BUILDDIR)/basic_type.cmx: src/initialization/types/basic_type.ml \
	$(BUILDDIR)/arithmetic_types.cmx \
	$(BUILDDIR)/graphic_types.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-open Arithmetic_types \
		-open Graphic_types


$(BUILDDIR)/locate_format.cmx: src/initialization/encodings/locate_format.ml | $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@


$(BUILDDIR)/variables.cmx: src/initialization/variables.ml \
	$(BUILDDIR)/complex_type.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-open Complex_type


$(BUILDDIR)/project_type.cmx: src/initialization/types/project_type.ml \
	$(BUILDDIR)/graphic_types.cmx \
	$(BUILDDIR)/variables.cmx \
	$(BUILDDIR)/locate_format.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-open Graphic_types \
		-open Variables \
		-open Locate_format


$(BUILDDIR)/basic_encoding.cmx: src/initialization/encodings/basic_encoding.ml \
	$(BUILDDIR)/bmp_reader.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-open Bmp_reader


$(BUILDDIR)/command_display.cmx: src/initialization/encodings/command_display.ml | $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@


$(BUILDDIR)/g1m_reader.cmx: src/g1m_read_write/g1m_reader.ml \
	$(BUILDDIR)/basic_encoding.cmx \
	$(BUILDDIR)/file_reader.cmx \
	$(BUILDDIR)/project_type.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-open Basic_encoding \
		-open File_reader \
		-open Project_type


$(BUILDDIR)/arithmetic_def.cmx: src/arithmetic/definitions/arithmetic_def.ml \
	$(BUILDDIR)/complex_type.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-open Complex_type


$(BUILDDIR)/entity_functions.cmx: src/arithmetic/definitions/entity_functions.ml \
	$(BUILDDIR)/complex_type.cmx \
	$(BUILDDIR)/arithmetic_types.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-open Complex_type \
		-open Arithmetic_types


$(BUILDDIR)/string_func.cmx: src/arithmetic/definitions/string_func.ml \
	$(BUILDDIR)/project_type.cmx \
	$(BUILDDIR)/arithmetic_types.cmx \
	$(BUILDDIR)/complex_type.cmx \
	$(BUILDDIR)/locate_format.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-open Project_type \
		-open Arithmetic_types \
		-open Complex_type \
		-open Locate_format


$(BUILDDIR)/arithmetic_lexing.cmx: src/arithmetic/arithmetic_lexing.ml \
	$(BUILDDIR)/arithmetic_def.cmx \
	$(BUILDDIR)/arithmetic_types.cmx \
	$(BUILDDIR)/variables.cmx \
	$(BUILDDIR)/complex_type.cmx \
	$(BUILDDIR)/string_func.cmx \
	$(BUILDDIR)/entity_functions.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-open Arithmetic_def \
		-open Arithmetic_types \
		-open Variables \
		-open Complex_type \
		-open String_func \
		-open Entity_functions


$(BUILDDIR)/arithmetic_evaluation.cmx: src/arithmetic/arithmetic_evaluation.ml \
	$(BUILDDIR)/arithmetic_types.cmx \
	$(BUILDDIR)/complex_type.cmx \
	$(BUILDDIR)/variables.cmx \
	$(BUILDDIR)/project_type.cmx \
	$(BUILDDIR)/key_check.cmx \
	$(BUILDDIR)/arithmetic_def.cmx \
	$(BUILDDIR)/entity_functions.cmx \
	$(BUILDDIR)/string_func.cmx \
	$(BUILDDIR)/arithmetic_lexing.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-open Arithmetic_types \
		-open Complex_type \
		-open Variables \
		-open Project_type \
		-open Key_check \
		-open Arithmetic_def \
		-open Entity_functions \
		-open String_func \
		-open Arithmetic_lexing


$(BUILDDIR)/data_structures.cmx: src/basic_compilation/initialization/data_structures.ml \
	$(BUILDDIR)/basic_type.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-open Basic_type


$(BUILDDIR)/compilation_error.cmx: src/basic_compilation/initialization/compilation_error.ml | $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@


$(BUILDDIR)/compile_aux.cmx: src/basic_compilation/compile_aux.ml \
	$(BUILDDIR)/compilation_error.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-open Compilation_error


$(BUILDDIR)/process_commands.cmx: src/basic_compilation/process_commands.ml \
	$(BUILDDIR)/arithmetic_lexing.cmx \
	$(BUILDDIR)/data_structures.cmx \
	$(BUILDDIR)/compilation_error.cmx \
	$(BUILDDIR)/basic_type.cmx \
	$(BUILDDIR)/variables.cmx \
	$(BUILDDIR)/graphic_types.cmx \
	$(BUILDDIR)/arithmetic_types.cmx \
	$(BUILDDIR)/complex_type.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-open Arithmetic_lexing \
		-open Data_structures \
		-open Compilation_error \
		-open Basic_type \
		-open Variables \
		-open Graphic_types \
		-open Arithmetic_types \
		-open Complex_type


$(BUILDDIR)/basic_compile.cmx: src/basic_compilation/basic_compile.ml \
	$(BUILDDIR)/basic_type.cmx \
	$(BUILDDIR)/data_structures.cmx \
	$(BUILDDIR)/compilation_error.cmx \
	$(BUILDDIR)/arithmetic_lexing.cmx \
	$(BUILDDIR)/variables.cmx \
	$(BUILDDIR)/complex_type.cmx \
	$(BUILDDIR)/arithmetic_types.cmx \
	$(BUILDDIR)/process_commands.cmx \
	$(BUILDDIR)/compile_aux.cmx \
	$(BUILDDIR)/graphic_types.cmx \
	$(BUILDDIR)/project_type.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-open Basic_type \
		-open Data_structures \
		-open Compilation_error \
		-open Arithmetic_lexing \
		-open Variables \
		-open Complex_type \
		-open Arithmetic_types \
		-open Process_commands \
		-open Compile_aux \
		-open Graphic_types \
		-open Project_type


$(BUILDDIR)/colors.cmx: src/initialization/graphics/colors.ml | $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@


$(BUILDDIR)/graphics_lib.cmx: src/initialization/graphics/graphics_lib.ml \
	$(BUILDDIR)/colors.cmx \
	$(BUILDDIR)/graphic_parameters.cmx \
	$(BUILDDIR)/bmp_reader.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-package sdl2 \
		-package sdl2_ttf \
		-linkpkg \
		-open Colors \
		-open Graphic_parameters \
		-open Bmp_reader


$(BUILDDIR)/float_repr.cmx: src/basic_execution/initialization/float_repr.ml \
	$(BUILDDIR)/complex_type.cmx \
	$(BUILDDIR)/locate_format.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-open Complex_type \
		-open Locate_format


$(BUILDDIR)/timer.cmx: src/basic_execution/initialization/timer.ml \
	$(BUILDDIR)/key_check.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-package sdl2 \
		-linkpkg \
		-open Key_check


$(BUILDDIR)/runtime_error.cmx: src/basic_execution/initialization/runtime_error.ml | $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@

	
$(BUILDDIR)/general.cmx: src/basic_execution/graphic_functions/general.ml \
	$(BUILDDIR)/graphics_lib.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-package sdl2 \
		-linkpkg \
		-open Graphics_lib


$(BUILDDIR)/text_mode.cmx: src/basic_execution/graphic_functions/text_mode.ml \
	$(BUILDDIR)/basic_encoding.cmx \
	$(BUILDDIR)/graphic_parameters.cmx \
	$(BUILDDIR)/graphics_lib.cmx \
	$(BUILDDIR)/colors.cmx \
	$(BUILDDIR)/complex_type.cmx \
	$(BUILDDIR)/float_repr.cmx \
	$(BUILDDIR)/locate_format.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-package sdl2 \
		-linkpkg \
		-open Basic_encoding \
		-open Graphic_parameters \
		-open Graphics_lib \
		-open Colors \
		-open Complex_type \
		-open Float_repr \
		-open Locate_format


$(BUILDDIR)/graphic_mode.cmx: src/basic_execution/graphic_functions/graphic_mode.ml \
	$(BUILDDIR)/basic_encoding.cmx \
	$(BUILDDIR)/project_type.cmx \
	$(BUILDDIR)/variables.cmx \
	$(BUILDDIR)/complex_type.cmx \
	$(BUILDDIR)/graphics_lib.cmx \
	$(BUILDDIR)/colors.cmx \
	$(BUILDDIR)/general.cmx \
	$(BUILDDIR)/graphic_parameters.cmx \
	$(BUILDDIR)/float_repr.cmx \
	$(BUILDDIR)/locate_format.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-package sdl2 \
		-linkpkg \
		-open Basic_encoding \
		-open Project_type \
		-open Variables \
		-open Complex_type \
		-open Graphics_lib \
		-open Colors \
		-open General \
		-open Graphic_parameters \
		-open Float_repr \
		-open Locate_format


$(BUILDDIR)/wait_press.cmx: src/basic_execution/graphic_functions/wait_press.ml \
	$(BUILDDIR)/key_check.cmx \
	$(BUILDDIR)/graphic_parameters.cmx \
	$(BUILDDIR)/text_mode.cmx \
	$(BUILDDIR)/graphic_mode.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-package sdl2 \
		-linkpkg \
		-open Key_check \
		-open Graphic_parameters \
		-open Text_mode \
		-open Graphic_mode


$(BUILDDIR)/menu.cmx: src/basic_execution/graphic_functions/menu.ml \
	$(BUILDDIR)/graphics_lib.cmx \
	$(BUILDDIR)/graphic_parameters.cmx \
	$(BUILDDIR)/text_mode.cmx \
	$(BUILDDIR)/colors.cmx \
	$(BUILDDIR)/project_type.cmx \
	$(BUILDDIR)/key_check.cmx \
	$(BUILDDIR)/wait_press.cmx \
	$(BUILDDIR)/graphic_mode.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-package sdl2 \
		-linkpkg \
		-open Graphics_lib \
		-open Graphic_parameters \
		-open Text_mode \
		-open Colors \
		-open Project_type \
		-open Key_check \
		-open Wait_press \
		-open Graphic_mode


$(BUILDDIR)/run_aux.cmx: src/basic_execution/auxiliary/run_aux.ml \
	$(BUILDDIR)/project_type.cmx \
	$(BUILDDIR)/arithmetic_types.cmx \
	$(BUILDDIR)/variables.cmx \
	$(BUILDDIR)/complex_type.cmx \
	$(BUILDDIR)/text_mode.cmx \
	$(BUILDDIR)/graphic_mode.cmx \
	$(BUILDDIR)/command_display.cmx \
	$(BUILDDIR)/locate_format.cmx \
	$(BUILDDIR)/graphics_lib.cmx \
	$(BUILDDIR)/wait_press.cmx \
	$(BUILDDIR)/graphic_parameters.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-package sdl2 \
		-linkpkg \
		-open Project_type \
		-open Arithmetic_types \
		-open Variables \
		-open Complex_type \
		-open Text_mode \
		-open Graphic_mode \
		-open Command_display \
		-open Locate_format \
		-open Graphics_lib \
		-open Wait_press \
		-open Graphic_parameters


$(BUILDDIR)/qmark.cmx: src/basic_execution/auxiliary/qmark.ml \
	$(BUILDDIR)/arithmetic_types.cmx \
	$(BUILDDIR)/wait_press.cmx \
	$(BUILDDIR)/key_check.cmx \
	$(BUILDDIR)/text_mode.cmx \
	$(BUILDDIR)/run_aux.cmx \
	$(BUILDDIR)/command_display.cmx \
	$(BUILDDIR)/locate_format.cmx \
	$(BUILDDIR)/graphic_parameters.cmx \
	$(BUILDDIR)/arithmetic_lexing.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-package sdl2 \
		-linkpkg \
		-open Arithmetic_types \
		-open Wait_press \
		-open Key_check \
		-open Text_mode \
		-open Run_aux \
		-open Command_display \
		-open Locate_format \
		-open Graphic_parameters \
		-open Arithmetic_lexing


$(BUILDDIR)/fline.cmx: src/basic_execution/auxiliary/graphics/fline.ml \
	$(BUILDDIR)/graphics_lib.cmx \
	$(BUILDDIR)/graphic_parameters.cmx \
	$(BUILDDIR)/graphic_types.cmx \
	$(BUILDDIR)/graphic_mode.cmx \
	$(BUILDDIR)/project_type.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-package sdl2 \
		-linkpkg \
		-open Graphics_lib \
		-open Graphic_parameters \
		-open Graphic_types \
		-open Graphic_mode \
		-open Project_type


$(BUILDDIR)/graphic_commands_aux.cmx: src/basic_execution/auxiliary/graphics/graphic_commands_aux.ml \
	$(BUILDDIR)/project_type.cmx \
	$(BUILDDIR)/arithmetic_types.cmx \
	$(BUILDDIR)/graphic_mode.cmx \
	$(BUILDDIR)/variables.cmx \
	$(BUILDDIR)/complex_type.cmx \
	$(BUILDDIR)/arithmetic_evaluation.cmx \
	$(BUILDDIR)/fline.cmx \
	$(BUILDDIR)/graphics_lib.cmx \
	$(BUILDDIR)/general.cmx \
	$(BUILDDIR)/graphic_types.cmx \
	$(BUILDDIR)/timer.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-package sdl2 \
		-package unix \
		-linkpkg \
		-open Project_type \
		-open Arithmetic_types \
		-open Graphic_mode \
		-open Variables \
		-open Complex_type \
		-open Arithmetic_evaluation \
		-open Fline \
		-open Graphics_lib \
		-open General \
		-open Graphic_types \
		-open Timer

$(BUILDDIR)/execute_graphic_commands.cmx: src/basic_execution/auxiliary/graphics/execute_graphic_commands.ml \
	$(BUILDDIR)/project_type.cmx \
	$(BUILDDIR)/graphic_types.cmx \
	$(BUILDDIR)/arithmetic_evaluation.cmx \
	$(BUILDDIR)/graphic_mode.cmx \
	$(BUILDDIR)/fline.cmx \
	$(BUILDDIR)/timer.cmx \
	$(BUILDDIR)/complex_type.cmx \
	$(BUILDDIR)/runtime_error.cmx \
	$(BUILDDIR)/graphics_lib.cmx \
	$(BUILDDIR)/text_mode.cmx \
	$(BUILDDIR)/wait_press.cmx \
	$(BUILDDIR)/run_aux.cmx \
	$(BUILDDIR)/variables.cmx \
	$(BUILDDIR)/general.cmx \
	$(BUILDDIR)/graphic_commands_aux.cmx \
	$(BUILDDIR)/arithmetic_types.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-package sdl2 \
		-package unix \
		-linkpkg \
		-open Project_type \
		-open Graphic_types \
		-open Arithmetic_evaluation \
		-open Graphic_mode \
		-open Fline \
		-open Timer \
		-open Complex_type \
		-open Runtime_error \
		-open Graphics_lib \
		-open Text_mode \
		-open Wait_press \
		-open Run_aux \
		-open Variables \
		-open General \
		-open Graphic_commands_aux \
		-open Arithmetic_types


$(BUILDDIR)/basic_run.cmx: src/basic_execution/basic_run.ml \
	$(BUILDDIR)/project_type.cmx \
	$(BUILDDIR)/basic_type.cmx \
	$(BUILDDIR)/complex_type.cmx \
	$(BUILDDIR)/key_check.cmx \
	$(BUILDDIR)/graphic_parameters.cmx \
	$(BUILDDIR)/text_mode.cmx \
	$(BUILDDIR)/graphic_mode.cmx \
	$(BUILDDIR)/run_aux.cmx \
	$(BUILDDIR)/timer.cmx \
	$(BUILDDIR)/colors.cmx \
	$(BUILDDIR)/arithmetic_evaluation.cmx \
	$(BUILDDIR)/qmark.cmx \
	$(BUILDDIR)/runtime_error.cmx \
	$(BUILDDIR)/variables.cmx \
	$(BUILDDIR)/general.cmx \
	$(BUILDDIR)/execute_graphic_commands.cmx \
	$(BUILDDIR)/float_repr.cmx \
	$(BUILDDIR)/locate_format.cmx \
	$(BUILDDIR)/arithmetic_types.cmx \
	$(BUILDDIR)/menu.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-package sdl2 \
		-package unix \
		-linkpkg \
		-open Project_type \
		-open Basic_type \
		-open Complex_type \
		-open Key_check \
		-open Graphic_parameters \
		-open Text_mode \
		-open Graphic_mode \
		-open Run_aux \
		-open Timer \
		-open Colors \
		-open Arithmetic_evaluation \
		-open Qmark \
		-open Runtime_error \
		-open Variables \
		-open General \
		-open Execute_graphic_commands \
		-open Float_repr \
		-open Locate_format \
		-open Arithmetic_types \
		-open Menu


$(BUILDDIR)/main_menu.cmx: src/main_menu/main_menu.ml \
	$(BUILDDIR)/text_mode.cmx \
	$(BUILDDIR)/graphics_lib.cmx \
	$(BUILDDIR)/g1m_reader.cmx \
	$(BUILDDIR)/colors.cmx \
	$(BUILDDIR)/graphic_parameters.cmx \
	$(BUILDDIR)/locate_format.cmx \
	$(BUILDDIR)/basic_type.cmx \
	$(BUILDDIR)/project_type.cmx \
	$(BUILDDIR)/general.cmx \
	$(BUILDDIR)/key_check.cmx \
	$(BUILDDIR)/graphic_mode.cmx \
	$(BUILDDIR)/variables.cmx \
	$(BUILDDIR)/wait_press.cmx \
	$(BUILDDIR)/basic_run.cmx \
	$(BUILDDIR)/run_aux.cmx \
	$(BUILDDIR)/file_reader.cmx \
	$(BUILDDIR)/basic_compile.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-package sdl2 \
		-linkpkg \
		-open Text_mode \
		-open Graphics_lib \
		-open G1m_reader \
		-open Colors \
		-open Graphic_parameters \
		-open Locate_format \
		-open Basic_type \
		-open Project_type \
		-open General \
		-open Key_check \
		-open Graphic_mode \
		-open Variables \
		-open Wait_press \
		-open Basic_run \
		-open Run_aux \
		-open File_reader \
		-open Basic_compile

$(BUILDDIR)/emulator_exec.cmx: src/emulator_exec.ml \
	$(BUILDDIR)/main_menu.cmx \
	$(BUILDDIR)/graphic_parameters.cmx \
	| $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c $< -o $@ \
		-package sdl2 \
		-linkpkg \
		-open Main_menu \
		-open Graphic_parameters

# Emulator executable
$(EXEC_EMULATOR): $(OBJS)
	@echo $(EXEC_EMULATOR)
	$(FINDOPT) -o $(EXEC_EMULATOR) $(OBJS) \
		-package unix \
		-package sdl2 \
		-package sdl2_ttf \
		-linkpkg

