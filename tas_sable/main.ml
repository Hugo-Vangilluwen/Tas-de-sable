open Tas_sable
open Grille_carree
open Grille_hexagonale
(* open Grille_montagne *)
open Grille_puit
open Grille_ligne

module A: Puit = struct
    let p = [(10, 10); (11, 10); (11, 11); (10, 11)]
end

module B: Puit = struct
    let p = [(5, 5)]
end

module Tsp = Tas_sable(Ajouter_puit (A) (Grille_carree))


let main: unit =
    (*
    let test = Tsc.creer (7, 7) in
    let test = Tsc.un_grain_clavier test (0, 0) 200 in
    Tsc.imprimer test
    *)

    let n = 19 in
    (*
    let debut = Sys.time() in
    let id = Tsc.identité (n, n) in
    let fin = Sys.time() in
    Printf.printf "Taille: %d\n" n;
    Printf.printf "Execution time: %fs" (fin -. debut);
    print_newline ();
    Tsc.afficher id;


    let debut = Sys.time() in
    let id = Tsp.identite (n, n) in
    let fin = Sys.time() in
    Printf.printf "Taille: %d\n" n;
    Printf.printf "Execution time: %fs" (fin -. debut);
    print_newline ();
    Tsp.afficher id
    *)

    let debut = Sys.time() in
    let id = Tsl.identite (n, 0) in
    let fin = Sys.time() in
    Printf.printf "Taille: %d\n" n;
    Printf.printf "Execution time: %fs" (fin -. debut);
    print_newline ();
    Tsl.imprimer id;
    Tsl.afficher id

    (*
    let source = Tsm.creer (20, 20) in
    Tsm.deposer source 1 (0, 0);
    let m = Tsm.(+) id source in
    let _ = Tsm.un_grain_temps id (5, 5) 5000 0.01 in
    ()
    *)
(*     (50, 50) |> Tsc.identite |> Tsc .afficher *)
