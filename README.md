# TIPE Tas de sable 2026

Ce dépôt contient mes fichiers pour le TIPE de 2026 autour des tas de sable.

`tas_sable` contient le code OCaml qui permet de simuler les tas de sable.
`documents` contient les différents documents lus dans le cadre de ce TIPE.
`Théorie` contient les sources LaTeX du document de synthèse.
`kadanoff` contient le code C et Python de Quentin pour simuler des tas de
sable de Kadanoff.

## Simulation

Le fichier `tas_sable/tas_sable.ml` contient le cœur de la simulation. 
Il définit une signature de module qui représente un graphe des tas de sable et
un foncteur qui à partir du type précédent, construit un tas de sable.

Chaque fichier `tas_sable/grille_*.ml` implémente un graphe des tas de sable
(* désigne `carree`, `ligne`, `complete`, `hexagonale` ...).
Pour utiliser un type de graphe particulier, il suffit d'importer le fichier
grille correspondant. Par exemple,
```ocaml
(* Affiche l'identité d'une grille carrée de 20x20 *)
open Grille_carree
Tsc.identite () (20, 20) |> Tsc.afficher

(* Affiche l'animation de 500 grains de sable déposés sur une grille hexagonale
 de 10x10 *)
open Grille_hexagonale
let tas_vide = Tsh.creer () (10, 10) in
Tsh.un_grain_temps () (4, 4) 500 0.05
```
