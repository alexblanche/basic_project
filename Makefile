OPT = @ocamlopt
FINDOPT = @ocamlfind opt
MKDIR = @mkdir

SUFFIXES ?= .ml .o .cmx .cmi
.SUFFIXES: $(SUFFIXES) .

EXEC_EMULATOR = main
BUILDDIR = ./build

# To do: add all the files
OBJS = $(BUILDDIR)/test1.cmx $(BUILDDIR)/test2draw.cmx $(BUILDDIR)/main.cmx

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
	@echo $@
	$(FINDOPT) -I $(BUILDDIR) -c fold1/test1.ml -o $@ -package unix -linkpkg 
	
$(BUILDDIR)/test2draw.cmx: fold2/test2draw.ml | $(BUILDDIR)
	@echo $@
	$(FINDOPT) -I $(BUILDDIR) -c fold2/test2draw.ml -o $@ -package unix -package sdl2 -linkpkg
	
$(BUILDDIR)/main.cmx: main.ml | $(BUILDDIR)
	@echo $@
	$(OPT) -I $(BUILDDIR) -c main.ml -o $@ -open Test1 -open Test2draw

# Emulator executable
$(EXEC_EMULATOR): $(OBJS)
	@echo $@
	$(FINDOPT) -o $(EXEC_EMULATOR) $(OBJS) -package unix -package sdl2 -linkpkg

