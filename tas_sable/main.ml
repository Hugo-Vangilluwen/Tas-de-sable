open Tas_sable
open Grille_hexagonale

let main: unit =
    let test = Tsh.créer (5, 5) in
    Tsh.modifier test 36 (4, 2);
    let test = test |> Tsh.avalanche in
    test |> Tsh.imprimer;
    test |> Tsh.afficher;
    List.iter
        (fun (x, y) -> print_int x; print_string ";"; print_int y; print_newline ())
        (Tsh.voisins test (0, 0))
