SHELL = /bin/sh
#Configurable options
TARGET=Main
OCB=ocamlbuild -ocamlc "ocamlfind c" -ocamlopt "ocamlfind opt" -lflags "-package unix","-linkpkg"
#YACC=menhir
MODE=byte
FTP_TARGET=till.varoquaux@login.free.fr:/projects/$(PROJECT_NAME)
#end configurable options

BUILDDIR=_build

ifeq ($(TERM),dumb)
	OCB += -classic-display
endif

ifeq ($(YACC),menhir)
	OCB += -use-menhir -yaccflags --explain
else
	OCB += -yaccflags -v
endif
MLI=$(wildcard *.mli)

all:$(MODE) annot check

Makefile.in:
	${error You must run configure before make...}

Makefile:Makefile.in

.PHONY:all clean byte opt dist doc check annot web

opt:sane
	${info * making native code}
	@$(OCB) $(TARGET).native

byte:sane
	${info * making byte code}
	@$(OCB) $(TARGET).byte

doc:$(MLI)
	${info * making docs...}
	lp4all -p "$(PROJECT_NAME)" -d doc $$(darcs query manifest)
#	@echo "$(basename $(MLI))" > doc.odocl
#	@$(OCB) doc.docdir/index.html

check:$(MODE)
	${info * runnning tests}
	@ocaml RunTests.ml

sane:
	@rm -f *.annot

annot:$(MODE)
	@cp _build/*.annot . 

web:
	${info "updating web site"}
	@rm -rf web
	@darcs get . --repo-name=web
	@cd web;\
	lp4all -p "$(PROJECT_NAME)" $$(darcs query manifest);\
	rm -f sparsetable.py parsetable.py;\
	lftp -c "open $(FTP_TARGET); mirror -Renv --parallel=5 "

clean:sane
	${info * cleaning up}
	@rm -rf web doc
	@rm -f *~ \#* doc.odocl parsetab.py sparsetab.py
	@$(OCB) -clean

distclean:clean
	@rm -f Makefile.in version.ml

dist:
	darcs dist -d "$(PROJECT_NAME)-$(VERSION)"

include Makefile.in
