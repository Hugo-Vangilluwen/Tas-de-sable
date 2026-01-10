open Tas_sable
open Grille_carree
open Grille_hexagonale
open Grille_montagne
open Grille_puit
open Grille_ligne

module Tsp = Tas_sable(Ajouter_puit (Grille_carree))


let main: unit =
    
    let test = Tsm.creer (Float.pi /. 3.0) (50, 50) in
    let _ = Tsm.un_grain_temps (Float.pi /. 3.0) test (25, 25) 100000 0.001 in
    ()

    (*let n = 40 in
    let debut = Sys.time() in
    let id = Tsm.identite (Float.pi /. 3.0) (2, 2) in
    let fin = Sys.time() in
    Printf.printf "Taille: %d\n" n;
    Printf.printf "Execution time: %fs" (fin -. debut);
    print_newline ();

    Tsm.afficher id;

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
    Tsh.afficher id*)

    (*
    let source = Tsm.creer (20, 20) in
    Tsm.deposer source 1 (0, 0);
    let m = Tsm.(+) id source in
    let _ = Tsm.un_grain_temps id (5, 5) 5000 0.01 in
    ()
    *)
(*     (50, 50) |> Tsc.identite |> Tsc .afficher *)
