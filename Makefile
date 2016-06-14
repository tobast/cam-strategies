OCB=ocamlbuild
OCBFLAGS=

TARGET=main.native
TARGET_DBG=main.d.byte
DOCDIR=doc.docdir

DOC=$(DOCDIR)/index.html

all: $(TARGET) doc
	
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
	
