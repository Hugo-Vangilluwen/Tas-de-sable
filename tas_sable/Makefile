CIBLE := tas_sable
BUILD_DIR := ./build

SOURCES := order.ml eltsI.ml elts.ml helpers.ml matrixI.ml matrix.ml tas_sable.ml grille_carree.ml grille_hexagonale.ml grille_puit.ml grille_ligne.ml
# grille_montagne.ml
OBJETS := $(SOURCES:%.ml=%.cmx)

OCAML_OPT := -package graphics -package unix


all: $(CIBLE)

$(CIBLE): $(SOURCES) main.ml
	ocamlfind ocamlopt -o $(CIBLE) -linkpkg $(OCAML_OPT) $(SOURCES) main.ml

# Décommenter pour complier les objets
# $(CIBLE): $(OBJETS)
# 	ocamlfind ocamlopt -o $(CIBLE) -linkpkg $(OCAML_OPT) $(OBJETS)

# %.cmx: %.ml
# 	ocamlfind ocamlopt -c $< $(OCAML_OPT)

test: $(SOURCES) test.ml
	ocamlfind ocamlopt -o test -linkpkg $(OCAML_OPT) $(SOURCES) test.ml


.PHONY: clean execute
clean:
	rm -f *.cmx *.cmi *.o $(CIBLE) test

run: $(CIBLE)
	./$(CIBLE)
