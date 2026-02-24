CIBLE := tas_sable
BUILD_DIR := ./build

MATRIX_LIB := order.ml eltsI.ml elts.ml helpers.ml matrixI.ml matrix.ml eltsQ.ml

GRILLES := grille_carree.ml grille_hexagonale.ml grille_puit.ml grille_ligne.ml grille_exemple.ml grille_montagne.ml grille_complete.ml

SOURCES := $(MATRIX_LIB) tas_sable.ml $(GRILLES)
OBJETS := $(SOURCES:%.ml=%.cmx)

OCAML_OPT := -package graphics,unix,zarith


all: $(CIBLE)

$(CIBLE): $(SOURCES) main.ml
	ocamlfind ocamlc -o $(CIBLE) -linkpkg $(OCAML_OPT) $(SOURCES) main.ml

# Décommenter pour complier les objets
# $(CIBLE): $(OBJETS)
# 	ocamlfind ocamlopt -o $(CIBLE) -linkpkg $(OCAML_OPT) $(OBJETS)

# %.cmx: %.ml
# 	ocamlfind ocamlopt -c $< $(OCAML_OPT)


doc: $(CIBLE)
	@if [ -e "doc" ]; then \
		if [ ! -d "doc" ]; then \
			echo "doc existe déjà comme fichier"; \
			exit 1; \
		fi \
	else \
		mkdir doc; \
	fi
	ocamlfind ocamldoc -html -d doc $(OCAML_OPT) $(SOURCES) main.ml


.PHONY: clean run test

test: $(SOURCES) test.ml
	ocamlfind ocamlopt -o test -linkpkg $(OCAML_OPT) $(SOURCES) test.ml
	./test

clean:
	rm -f *.cm[xio] *.o $(CIBLE) test
	rm -rf doc

run: $(CIBLE)
	./$(CIBLE)
