(* Ajoute un puit dans une grille *)

open Tas_sable

module type Puit = sig
    val p : coord
end

module Ajouter_puit (P: Puit) (G: GRILLE): GRILLE = struct
    type t = G.t

    let max_valeur (g: t) (c: coord) =
        G.max_valeur g c - (if List.mem c (G.voisins g P.p) then 1 else 0)

    let créer (dim: coord): t =
        let g = G.créer dim in
        assert (G.correcte_coord g P.p);
        g

    let valeur = G.valeur

    let déposer (g: t) (n: int) (c: coord): unit =
        if c <> P.p then G.déposer g n c

    let correcte_coord (g: t) (c: coord): bool =
        c <> P.p && G.correcte_coord g c

    let voisins (g: t) (c: coord): coord list =
        List.filter ((<>) P.p) (G.voisins g c)

    let copier = G.copier

    let dimensions = G.dimensions

    let superposer = G.superposer

    let itérer (f: coord -> unit) (g: t): unit =
        G.itérer (fun c -> if c <> P.p then f c) g

    let imprimer = G.imprimer

    let ouvrir_fenêtre = G.ouvrir_fenêtre

    let afficher_grille = G.afficher_grille
end
