open Tas_sable
open Grille_carree
open Grille_hexagonale
(* open Grille_montagne *)
open Grille_puit
open Grille_ligne

module Tsp = Tas_sable(Ajouter_puit (Grille_carree))


let main: unit =
    (*
    let test = Tsc.creer (7, 7) in
    let test = Tsc.un_grain_clavier test (0, 0) 200 in
    Tsc.imprimer test
    *)

    let n = 19 in

    let debut = Sys.time() in
    let id = Tsc.identite () (n, n) in
    let fin = Sys.time() in
    Printf.printf "Taille: %d\n" n;
    Printf.printf "Execution time: %fs" (fin -. debut);
    print_newline ();
    Tsc.afficher id;

    Tsc.mettre_dim_cases 20;
    (50, 40) |> (Tsc.identite ()) |> Tsc.afficher;

    let debut = Sys.time() in
    let id = Tsp.identite ([(5, 5)], ()) (n, n) in
    let fin = Sys.time() in
    Printf.printf "Taille: %d\n" n;
    Printf.printf "Execution time: %fs" (fin -. debut);
    print_newline ();
    Tsp.afficher id;

    let debut = Sys.time() in
    let id = Tsh.identite () (n, n) in
    let fin = Sys.time() in
    Printf.printf "Taille: %d\n" n;
    Printf.printf "Execution time: %fs" (fin -. debut);
    print_newline ();
    Tsh.afficher id

    (*
    let source = Tsm.creer (20, 20) in
    Tsm.deposer source 1 (0, 0);
    let m = Tsm.(+) id source in
    let _ = Tsm.un_grain_temps id (5, 5) 5000 0.01 in
    ()
    *)
