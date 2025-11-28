(* Ajoute un puit dans une grille *)

open Tas_sable

module type Puit = sig
    val p : coord list
end

module Ajouter_puit (P: Puit) (G: GRILLE): GRILLE = struct
    type t = G.t

    let max_valeur (g: t) (c: coord) =
        let v = G.voisins g c in
        G.max_valeur g c -
        (List.fold_left
            (fun acc puit -> if List.mem puit v then acc + 1 else acc)
            0
            P.p )

    let créer (dim: coord): t =
        let g = G.créer dim in
        List.iter (fun puit -> assert (G.correcte_coord g puit)) P.p;
        g

    let valeur = G.valeur

    (* Teste si la coordonnée n'est pas un puit *)
    let est_pas_puit (c: coord): bool =
        List.for_all ((<>) c) P.p

    let déposer (g: t) (n: int) (c: coord): unit =
        if est_pas_puit c then G.déposer g n c

    let correcte_coord (g: t) (c: coord): bool =
        est_pas_puit c && G.correcte_coord g c

    let voisins (g: t) (c: coord): coord list =
        List.filter est_pas_puit (G.voisins g c)

    let copier = G.copier

    let dimensions = G.dimensions

    let superposer = G.superposer

    let itérer (f: coord -> unit) (g: t): unit =
        G.itérer (fun c -> if est_pas_puit c then f c) g

    let imprimer = G.imprimer

    let ouvrir_fenêtre = G.ouvrir_fenêtre

    let afficher_grille = G.afficher_grille
end
