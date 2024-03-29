MODULES=rationals matrix arrayMatrix test ast main parser lexer
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
EXAMPLE=example.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -use-menhir 
PKGS=unix,oUnit,str,qcheck,ANSITerminal

default: build
	utop

generate-parser:
	$(OCAMLBUILD) lexer.ml $(OBJECTS) \
	$(OCAMLBUILD) parser.ml $(OBJECTS)

run:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

example:
	$(OCAMLBUILD) $(EXAMPLE) && ./$(EXAMPLE)

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag debug $(TEST) && ./$(TEST)

zip:
	zip oalgebra_src.zip *.ml* *.mll* *.mly* *.txt* _tags Makefile

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report oalgebra_src.zip bisect*.out

bisect-test:
	$(OCAMLBUILD) -package bisect -syntax camlp4o,bisect_pp \
	  $(TEST) && ./$(TEST) -runner sequential

bisect: clean bisect-test
	bisect-report -I _build -html report bisect0001.out

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-m A -hide-warnings $(MLIS) $(MLS)

