SHELL = /bin/sh
TARGET=Main
OCAMLBUILD=ocamlbuild

OCB=$(OCAMLBUILD) -cflags -warn-error,A,-dtypes

ifeq ($(TERM),dumb)
	OCB += -classic-display
endif

MLI=$(wildcard *.mli)

export PATH := $(shell dirname $$(which camlp4rf)):$(PATH)

all:byte doc test

.PHONY:all clean byte opt dist doc test

opt:
	${info * making native code}
	@$(OCB) $(TARGET).native

byte:
	${info * making byte code}
	@$(OCB) $(TARGET).byte

doc:$(MLI)
	${info * making docs...}
	@echo "$(basename $(MLI))" > doc.odocl
	@$(OCB) doc.docdir/index.html

test:
	${info * runnning tests}
	@ocaml RunTests.ml

clean:
	${info * cleaning up}
	@rm -f *~ \#* doc.odocl
	@$(OCB) -clean

dist:
	darcs dist
