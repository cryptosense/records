.PHONY: all install uninstall clean check
PACKAGE=records
MLI=record type polid
OBJ=$(addprefix _build/, $(addsuffix .cmi, $(MLI)) $(PACKAGE).cma $(PACKAGE).cmxa $(PACKAGE).a)

all: $(OBJ)

check:
	ocamlbuild -use-ocamlfind tests.native --

_build/%:
	ocamlbuild -use-ocamlfind $*

install: uninstall
	ocamlfind install $(PACKAGE) $(OBJ) META

uninstall:
	ocamlfind remove $(PACKAGE)

clean:
	ocamlbuild -clean
