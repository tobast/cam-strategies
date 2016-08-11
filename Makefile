# You can select which toplevel you want by putting its name in 'ocamltop'.
OCAML= \
	$(shell if [ -f ./ocamltop ] ; then cat ./ocamltop; else echo 'ocaml'; fi)
OCB=ocamlbuild
JS_OF_OCAML=js_of_ocaml
OCBFLAGS=-use-ocamlfind

WEBDIR=web/
TARGET=$(WEBDIR)web.js
TARGET_DBG=main.d.byte
TARGET_BYTE=main.byte
DOCDIR=doc.docdir
BUILD=_build/

REMOTE=www.tobast
REMOTE_PATH=~/tobast.fr/public_html/l3/

OBJS=\
	$(BUILD)datatypes.cmo \
	$(BUILD)modExtensions.cmo \
	$(BUILD)helpers.cmo \
	$(BUILD)builder.cmo \
	$(BUILD)printer.cmo \
	$(BUILD)operations/pullback.cmo \
	$(BUILD)operations/parallel.cmo \
	$(BUILD)operations/composition.cmo \
	$(BUILD)operations.cmo \
	$(BUILD)lang/stratlangLexer.cmo \
	$(BUILD)lang/stratlangParser.cmo \
	$(BUILD)lang/langReader.cmo \
	$(BUILD)linearLambda.cmo

TL_OBJS=\
	unix.cma

TL_OPENED_MODULES=Helpers Datatypes Builder Printer Operations Canonical \
	LangReader

TL_LLAM_OPENED_MODULES=$(TL_OPENED_MODULES) LinearLambda LlamHelpers \
	LlamPrinter LlamInterpret

INCLUDEDIRS= . operations lang

###############################################################################

DOC=$(DOCDIR)/index.html
TL_OPEN_CMD= $(patsubst %,-open %,$(TL_OPENED_MODULES))
TL_LLAM_OPEN_CMD= $(patsubst %,-open %,$(TL_LLAM_OPENED_MODULES))
TL_INCLUDE_CMD= $(patsubst %,-I $(BUILD)%,$(INCLUDEDIRS))

all: $(TARGET) $(TARGET_BYTE) doc

debug: $(TARGET_DBG)

doc: $(DOC)

.PHONY: buildanyway_
buildanyway_: ;

%.docdir/index.html: %.odocl buildanyway_
	$(OCB) $(OCBFLAGS) $@

%.native: %.ml buildanyway_
	$(OCB) $(OCBFLAGS) $@

%.byte: %.ml buildanyway_
	$(OCB) $(OCBFLAGS) $@

%.p.native: %.ml buildanyway_
	$(OCB) $(OCBFLAGS) $@

%.d.byte: %.ml buildanyway_
	$(OCB) $(OCBFLAGS) $@

%.js: %.byte
	$(JS_OF_OCAML) -o "$@" $(BUILD)$<

clean:
	$(OCB) -clean
	rm -f $(TARGET)

cleandoc:
	rm -rf _build/$(DOCDIR)

toplevelcmd: $(TARGET_BYTE)
	@echo '$(OCAML) $(TL_INCLUDE_CMD) $(TL_OPEN_CMD) $(OBJS)'

toplevel: $(TARGET_BYTE)
	$(OCAML) $(TL_INCLUDE_CMD) $(TL_OPEN_CMD) $(TL_OBJS) $(OBJS)

toplevel-llam: $(TARGET_BYTE)
	$(OCAML) $(TL_INCLUDE_CMD) $(TL_LLAM_OPEN_CMD) $(TL_OBJS) $(OBJS)

upload: $(TARGET)
	scp -r $(WEBDIR)* $(REMOTE):$(REMOTE_PATH)

