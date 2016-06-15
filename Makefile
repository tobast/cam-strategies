# You can select which toplevel you want by putting its name in 'ocamltop'.
OCAML= \
	$(shell if [ -f ./ocamltop ] ; then cat ./ocamltop; else echo 'ocaml'; fi)
OCB=ocamlbuild
OCBFLAGS=

TARGET=main.native
TARGET_DBG=main.d.byte
DOCDIR=doc.docdir
BUILD=_build/

OBJS=\
	$(BUILD)datatypes.cmo \
	$(BUILD)helpers.cmo \
	$(BUILD)builder.cmo \
	$(BUILD)printer.cmo \
	$(BUILD)operations/pullback.cmo \
	$(BUILD)operations.cmo

TL_OPENED_MODULES=Helpers,Datatypes,Builder,Printer,Operations

###############################################################################

DOC=$(DOCDIR)/index.html
comma= ,
TL_OPEN_CMD=-open $(subst $(comma), -open ,$(TL_OPENED_MODULES))

all: $(TARGET) $(TARGET_BYTE) doc
	
debug: $(TARGET_DBG)
	
doc: $(DOC)
	
%.docdir/index.html: %.odocl
	$(OCB) $@

%.native: %.ml
	$(OCB) $(OCBFLAGS) $@
	
%.byte: %.ml
	$(OCB) $(OCBFLAGS) $@

%.p.native: %.ml
	$(OCB) $(OCBFLAGS) $@

%.d.byte: %.ml
	$(OCB) $(OCBFLAGS) $@
	
clean:
	$(OCB) -clean

cleandoc:
	rm -rf _build/$(DOCDIR)
	
toplevel: $(TARGET_BYTE)
	$(OCAML) -I $(BUILD) -I $(BUILD)operations/ $(TL_OPEN_CMD) $(OBJS)
