.PHONY: all byte opt install uninstall clean check cov ppx
PACKAGE=records
MLI=record type polid
OBJ=$(addprefix _build/, $(addsuffix .cmi, $(MLI)) $(PACKAGE).cma)
NATIVE_OBJ=$(addprefix _build/, $(PACKAGE).cmxa $(PACKAGE).a $(PACKAGE).cmxs)
PPX=_build/ppx_deriving_typ.cmxs

all: byte opt

byte: $(OBJ)

opt: $(NATIVE_OBJ)

ppx: $(PPX)

check:
	ocamlbuild -use-ocamlfind tests.native --

tests.cov:
	rm -f tests.native
	ocamlbuild -use-ocamlfind -package bisect_ppx tests.native
	mv tests.native $@

cov: tests.cov
	./tests.cov -runner sequential
	cd _build ; bisect-ppx-report -summary-only -text /dev/stdout ../bisect000*.out ; cd ..

_build/%:
	ocamlbuild -use-ocamlfind $*

install: uninstall
	ocamlfind install $(PACKAGE) META $(OBJ) *.mli -optional $(NATIVE_OBJ) _build/*.cmt _build/*.cmti $(PPX)

uninstall:
	ocamlfind remove $(PACKAGE)

clean:
	ocamlbuild -clean
	rm -rf bisect*.out tests.cov
