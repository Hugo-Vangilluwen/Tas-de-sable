CIBLE := tas_sable
BUILD_DIR := ./build

SOURCES := tas_sable.ml grille_carree.ml grille_hexagonale.ml grille_montagne.ml main.ml
OBJETS := $(SOURCES:%.ml=%.cmx)

OCAML_OPT := -package graphics -package unix


all: $(CIBLE)

$(CIBLE): $(SOURCES)
	ocamlfind ocamlopt -o $(CIBLE) -linkpkg $(OCAML_OPT) $(SOURCES)

# $(CIBLE): $(OBJETS)
# 	ocamlfind ocamlopt -o $(CIBLE) -linkpkg $(OCAML_OPT) $(OBJETS)

# %.cmx: %.ml
# 	ocamlfind ocamlopt -c $< $(OCAML_OPT)


.PHONY: clean execute
clean:
	rm -f *.cmx *.cmi *.o $(CIBLE)

execute: $(CIBLE)
	./$(CIBLE)
