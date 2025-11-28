open Tas_sable
open Grille_carree
open Grille_hexagonale
(* open Grille_montagne *)
open Grille_puit

module A: Puit = struct
    let p = [(20, 20); (21, 20); (21, 21); (20, 21)]
end

module Tsp = Tas_sable(Ajouter_puit (A) (Grille_hexagonale))

let main: unit =
    (*
    let test = Tsc.creer (7, 7) in
    let test = Tsc.un_grain_clavier test (0, 0) 200 in
    Tsc.imprimer test
    *)

    let n = 40 in
    let debut = Sys.time() in
    let id = Tsp.identite (n, n) in
    let fin = Sys.time() in
    Printf.printf "Taille: %d\n" n;
    Printf.printf "Execution time: %fs" (fin -. debut);
    print_newline ();
(*     Tsm.imprimer id; *)
    Tsp.afficher id

    (*
    let source = Tsm.creer (20, 20) in
    Tsm.deposer source 1 (0, 0);
    let m = Tsm.(+) id source in
    let _ = Tsm.un_grain_temps id (5, 5) 5000 0.01 in
    ()
    *)
(*     (50, 50) |> Tsc.identite |> Tsc .afficher *)
