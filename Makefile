SHELL = /bin/sh
TARGET=Main
OCAMLBUILD=ocamlbuild

BUILDDIR=_build
MODE=byte
OCB=$(OCAMLBUILD) -j 0 -cflags -warn-error,a,-dtypes

ifeq ($(TERM),dumb)
	OCB += -classic-display
endif

MLI=$(wildcard *.mli)

all:$(MODE) doc annot check

.PHONY:all clean byte opt dist doc check annot

opt:sane
	${info * making native code}
	@$(OCB) $(TARGET).native

byte:sane
	${info * making byte code}
	@$(OCB) $(TARGET).byte

doc:$(MLI)
	${info * making docs...}
	@echo "$(basename $(MLI))" > doc.odocl
	@$(OCB) doc.docdir/index.html

check:$(MODE)
	${info * runnning tests}
	@ocaml RunTests.ml

sane:
	@rm -f *.annot

annot:$(MODE)
	@cp _build/*.annot . 

clean:sane
	${info * cleaning up}
	@rm -f *~ \#* doc.odocl
	@$(OCB) -clean

dist:
	darcs dist
