.PHONY: all install uninstall clean
PACKAGE=records
MLI=record type polid
OBJ=$(addprefix _build/, $(addsuffix .cmi, $(MLI)) $(PACKAGE).cma $(PACKAGE).cmxa $(PACKAGE).a)

all: $(OBJ)

_build/%:
	ocamlbuild -use-ocamlfind $*

install: uninstall
	ocamlfind install $(PACKAGE) $(OBJ) META

uninstall:
	ocamlfind remove $(PACKAGE)

clean:
	ocamlbuild -clean
