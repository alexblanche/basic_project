OPT = @ocamlopt
FINDOPT = @ocamlfind opt
MKDIR = @mkdir
PRINT = @echo $@

SUFFIXES ?= .ml .o .cmx .cmi
.SUFFIXES: $(SUFFIXES) .

EXEC_EMULATOR = main
BUILDDIR = ./build

OBJS = $(BUILDDIR)/graphic_parameters.cmx \
$(BUILDDIR)/events.cmx \
$(BUILDDIR)/key_check.cmx \
$(BUILDDIR)/file_reader.cmx \
$(BUILDDIR)/bmp_reader.cmx \
$(BUILDDIR)/complex.cmx \
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
$(BUILDDIR)/main_menu.cmx

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

# To do
$(BUILDDIR)/test1.cmx: fold1/test1.ml | $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c fold1/test1.ml -o $@ -package unix -linkpkg 
	
$(BUILDDIR)/test2draw.cmx: fold2/test2draw.ml | $(BUILDDIR)
	$(PRINT)
	$(FINDOPT) -I $(BUILDDIR) -c fold2/test2draw.ml -o $@ -package unix -package sdl2 -linkpkg
	
$(BUILDDIR)/main.cmx: main.ml | $(BUILDDIR)
	$(PRINT)
	$(OPT) -I $(BUILDDIR) -c main.ml -o $@ -open Test1 -open Test2draw

# Emulator executable
$(EXEC_EMULATOR): $(OBJS)
	$(PRINT)
	$(FINDOPT) -o $(EXEC_EMULATOR) $(OBJS) -package unix -package sdl2 -linkpkg

