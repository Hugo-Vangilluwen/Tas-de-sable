open Tas_sable
open Grille_carree
open Grille_hexagonale
open Grille_montagne
open Grille_puit
open Grille_ligne
open Grille_complete

open EltsQ

module Tsp = Tas_sable(Ajouter_puit (Grille_carree))


let main: unit =
    let tas = (600, 600) |> (Tsh.identite ()) in
    Tsh.mettre_taille_cases 1;
    tas |> Tsh.afficher;
    Tsh.imprimer_liste_python tas
