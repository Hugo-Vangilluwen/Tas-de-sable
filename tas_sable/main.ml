open Tas_sable
open Grille_carree
open Grille_hexagonale
open Grille_montagne

let main: unit =
    (*let test = Tsh.créer (7, 7) in
    let test = Tsh.un_grain_temps test (0, 0) 200 in
    Tsh.imprimer test
    let test = Tsh.un_grain_temps test (0, 0) 10 in
    Tsh.imprimer test;*)
    let t = Sys.time() in
    let id = Tsm.identité (20, 20) in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
(*     Tsm.imprimer id; *)
    Tsm.afficher id;
    let source = Tsm.créer (20, 20) in
    Tsm.déposer source 1 (0, 0);
    let m = Tsm.(+) id source in
    let _ = Tsm.un_grain_temps id (5, 5) 50000 0.0001 in
    ()
