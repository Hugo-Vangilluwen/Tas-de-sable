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
    let source = Tsc.creer () (40, 40) in
    Tsc.deposer source 1 (15, 15);
    Tsc.deposer source 1 (34, 26);
    let attendre1 () =
         Unix.sleepf 0.001
    in
    let attendre2 () =
        let _ = Graphics.wait_next_event[Key_pressed] in ()
    in
    let tas1 = Tsc.creer () (40, 40) in
    Tsc.mettre_dim_cases 20;
    let tas2 = Tsc.animer tas1 1000 source attendre1 in
    let tas3 = Tsc.animer tas2 500 source attendre2 in ()

(*
    for n = 1 to 20 do
        let c = (n, n) in
        let card_rec = Tsc.cardinal_recurrents () c in
(*         let card_stab = Tsh.cardinal_stables () c in *)
        print_int n;
        print_string ": ";
        print_int (card_rec);
        let graphe = Tsc.creer () c in
        let rapport_rec_stab = Tsc.reduire graphe
            (fun acc c -> Q.div acc (Q.of_int (Tsc.max_valeur graphe c + 1)))
            (Q.of_int card_rec)
            in
(*         print_int (card_stab); *)
(*         print_string " = "; *)
        print_string " / ";
        print_float (Q.to_float rapport_rec_stab);
        print_newline ()
(*         RationnalMatrix.print (Tsc.laplacien_reduit () (n,n)) *)
    done;
*)

(*     let _ = Tsc.un_grain_temps () (Tscomplet.creer () (10, 0)) (4, 0) 123 0.05 in () *)
(*     in Graphics.close_graph () *)
(*     let id = Tsm.identite 0.5 (10, 10) in *)
(*     Tsm.afficher id *)

    (*
    let source = Tsm.creer (20, 20) in
    Tsm.deposer source 1 (0, 0);
    let m = Tsm.(+) id source in
    let _ = Tsm.un_grain_temps id (5, 5) 5000 0.01 in
    ()
    *)
