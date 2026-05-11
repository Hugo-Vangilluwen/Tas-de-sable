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

    (*let tas = Tsc.creer () (90, 90) in
    let (tot, r) = Tsc.avalanche_n_grains tas (25,25) 10050 in
    print_int r; print_string "\n"
    (*Tsc.deposer source 1 (15, 15);
    Tsc.deposer source 1 (34, 26);
    let attendre1 () =
         Unix.sleepf 0.001
    in
    let attendre2 () =
        let _ = Graphics.wait_next_event[Key_pressed] in ()
    in
    let tas1 = Tsc.creer () (40, 40) in
    Tsc.mettre_tailles_cases 20;
    let tas2 = Tsc.animer tas1 1000 source attendre1 in
    let tas3 = Tsc.animer tas2 500 source attendre2 in ()*)*)



    (*for n = 1 to 5 do
        let c = (n, n) in
        let card_rec, rapport_rec_stab =
            Tsc.rapport_recurrents_stables () c in
        print_int n;
        print_string ": ";
        Z.print (card_rec);
        print_string " / ";
        print_float (Q.to_float rapport_rec_stab);
        print_newline ();
        RationnalMatrix.print (Tsc.laplacien_reduit () c);
        Tsc.identite () c |> Tsc.afficher
    done*)

   (*      let _ = Tsc.un_grain_temps () (Tscomplet.creer () (10, 0)) (4, 0) 123 0.05 in ()
        in Graphics.close_graph () *)
        let id = Tsc.identite () (73, 73) in
        Tsc.afficher id

    (*
    let source = Tsm.creer (20, 20) in
    Tsm.deposer source 1 (0, 0);
    let m = Tsm.(+) id source in
    let _ = Tsm.un_grain_temps id (5, 5) 5000 0.01 in
    ()
    *)
