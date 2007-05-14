TARGET=Main
OCAMLBUILD=ocamlbuild

OCB=$(OCAMLBUILD) -cflags -warn-error,A -classic-display -no-log -no-hygiene

PARSERS=$(wildcard *.parse)
MLYGEN=$(PARSERS:.parse=.mly)

MLI=$(wildcard *.mli)
DEPS=$(wildcard *.ml*) OpenTrees.cmo $(MLYGEN)

export PATH := $(shell dirname $$(which camlp4rf)):$(PATH)

all:byte doc

.PHONY:all clean byte opt dist doc

%.cmo:%.ml
	ocamlc -c -pp "camlp4oof" -I +camlp4 -dtypes $<

%.mly:%.parse
	 sed -e 's/\([a-z].*\):$$/\1:\n|a_\1  { ParseInfo.setCurrentRule "\1"; $$1 }\na_\1:/' $< > $@

opt:$(DEPS)
	$(OCB) $(TARGET).native

byte:$(DEPS)
	$(OCB) $(TARGET).byte

doc:$(MLI)
	@echo "$(MLI:.mli=\n)" > doc.odocl
	$(OCB) doc.docdir/index.html
	@rm -rf doc.odocl

clean:
	rm -f *~ \#*
	rm -f *.cm* *.o *.annot
	rm -f $(MLYGEN)
	$(OCB) -clean
info:

dist:
	darcs dist
