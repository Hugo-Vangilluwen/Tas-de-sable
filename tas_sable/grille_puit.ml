(* Ajoute un puit dans une grille *)

open Tas_sable

module Ajouter_puit (G: GRILLE): GRILLE with type param = coord list * G.param = struct
    type param = coord list * G.param

    type t = {
        grille_int: G.t; (* grille interne *)
        puits: coord list;
    }

    let max_valeur (g: t) (c: coord) =
        let v = G.voisins g.grille_int c in
        G.max_valeur g.grille_int c -
        (List.fold_left
            (fun acc puit -> if List.mem puit v then acc + 1 else acc)
            0
            g.puits )

    let creer ((puits, param_int): param) (dim: coord): t =
        let g = G.creer param_int dim in
        List.iter (fun puit -> assert (G.correcte_coord g puit)) puits;
        (* Trier est nécessaire pour tester tester l'égalité des puits *)
        {
            grille_int = g;
            puits = List.sort compare puits;
        }

    let nb_cases (g: t): int =
        (G.nb_cases g.grille_int) - (List.length g.puits)

    let lineariser (g: t) (c: coord): int =
        let lin_c = G.lineariser g.grille_int c in
        lin_c -
        List.length (List.filter
            (fun p -> G.lineariser g.grille_int p < lin_c)
            g.puits)

    let valeur (g: t) = G.valeur g.grille_int

    (* Teste si la coordonnee n'est pas un puit *)
    let est_pas_puit (g: t) (c: coord): bool =
        List.for_all ((<>) c) g.puits

    let deposer (g: t) (n: int) (c: coord): unit =
        if est_pas_puit g c then G.deposer g.grille_int n c

    let correcte_coord (g: t) (c: coord): bool =
        est_pas_puit g c && G.correcte_coord g.grille_int c

    let voisins (g: t) (c: coord): coord list =
        List.filter (est_pas_puit g) (G.voisins g.grille_int c)

    let copier (g: t) =
        {
            grille_int = G.copier g.grille_int;
            puits = List.map (fun x -> x) g.puits;
        }

    let dimensions (g: t) = G.dimensions g.grille_int

    let superposer (g1: t) (g2: t) =
        assert (g1.puits = g2.puits); (* Les puits sont triés dans creer *)
        {
            grille_int = G.superposer g1.grille_int g2.grille_int;
            puits = g1.puits;
        }

    let iterer (f: coord -> unit) (g: t): unit =
        G.iterer (fun c -> if List.mem c g.puits then () else f c) g.grille_int

    let imprimer (g: t) = G.imprimer g.grille_int

    let mettre_taille_cases = G.mettre_taille_cases

    let couleur_case (g: t) (c: coord): Graphics.color =
        G.couleur_case g.grille_int c

    let taille_fenetre (g: t) = G.taille_fenetre g.grille_int

    let afficher_case (g: t) = G.afficher_case g.grille_int
end
