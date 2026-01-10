open Tas_sable
open Grille_carree
open Grille_hexagonale
(* open Grille_montagne *)
open Grille_puit
open Grille_ligne

module Test_tas_sable (G: GRILLE): sig
    val test: G.param -> coord -> unit
end = struct
    module Ts = Tas_sable(G)

    let test (p: G.param) (c: coord): unit =
        let debut = Sys.time() in
        let id = Ts.identite p c in
        let fin = Sys.time() in
        Printf.printf "Taille: %d, %d\n" (fst c) (snd c);
        Printf.printf "Temps d'execution: %fs" (fin -. debut);
        print_newline ();
        Ts.afficher id
end

module Test_carree = Test_tas_sable(Grille_carree)
module Test_hexagonale = Test_tas_sable(Grille_hexagonale)
(* module Test_montage = Test_tas_sable(Grille_montagne) *)
module Test_ligne = Test_tas_sable(Grille_ligne)

module Test_puit_carree = Test_tas_sable(Ajouter_puit (Grille_carree))

open Matrix.EltMatrix
open Grille_exemple
module Test_exemple = Test_tas_sable(Grille_exemple)

let test_laplacien: unit =
    let laplacien_exemple = Tse.laplacien_reduit () (2, 0) in
    let resultat = from_list [[6.; -3.]; [-5.; 5.]] in
    assert (laplacien_exemple = resultat);
    assert (Tse.cardinal_recurrents () (2, 0) = 15)


let main_test: unit =
    Test_carree.test () (50, 50);
    Test_hexagonale.test () (40, 40);
    Test_ligne.test () (21, 0);
    Test_exemple.test () (2, 0);
    Test_puit_carree.test ([(15, 15); (14, 15)], ()) (50, 50);
