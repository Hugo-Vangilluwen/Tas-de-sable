open Tas_sable
open Grille_carree
open Grille_hexagonale
(* open Grille_montagne *)
open Grille_puit
open Grille_ligne

open EltsQ

module Tsp = Tas_sable(Ajouter_puit (Grille_carree))


let main: unit =
    for n = 1 to 10 do
        print_int n;
        print_string ": ";'
        print_int (Tsc.cardinal_recurrents () (n, n));
        print_newline ();
(*         RationnalMatrix.print (Tsc.laplacien_reduit () (n,n)); *)
    done

    (*
    let source = Tsm.creer (20, 20) in
    Tsm.deposer source 1 (0, 0);
    let m = Tsm.(+) id source in
    let _ = Tsm.un_grain_temps id (5, 5) 5000 0.01 in
    ()
    *)
