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
    for i = 1 to 20 do
        let _, rho = Tsh.densite_recurrente () (i,i) in
        print_int i;
        print_string ": ";
        print_float (Q.to_float rho);
        print_newline ()
    done
