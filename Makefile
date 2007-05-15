TARGET=Main
OCAMLBUILD=ocamlbuild

OCB=$(OCAMLBUILD) -cflags -warn-error,A -classic-display -no-log

MLI=$(wildcard *.mli)
DEPS=$(wildcard *.ml*)

export PATH := $(shell dirname $$(which camlp4rf)):$(PATH)

all:byte doc

.PHONY:all clean byte opt dist doc

opt:$(DEPS)
	$(OCB) $(TARGET).native

byte:$(DEPS)
	$(OCB) $(TARGET).byte

doc:$(MLI)
	@echo "$(MLI:.mli=)" > doc.odocl
	$(OCB) doc.docdir/index.html
	@rm -rf doc.odocl

clean:
	rm -f *~ \#*
	$(OCB) -clean
info:

dist:
	darcs dist
