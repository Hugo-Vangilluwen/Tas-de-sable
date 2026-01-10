(* Modélise le multigraphe de l'exemple 6.14 du livre
 * Divisors and Sandpiles: An Introduction to Chip-Firing
 * de Scott David et Corry Perkinson
 *)

open Tas_sable

(* Cette erreur est levé quand la coordonnée en entrée n'est pas
 * (0, 0) ou (1, 0)
 *)
exception NiuNiv

module Grille_exemple: GRILLE with type param = unit = struct
    type param = unit

    type t = {
        mutable u: int; (* 0 *)
        mutable v: int; (* 1 *)
    }

    let max_valeur (_: t) ((x, y): coord): int =
        if x = 0 then 6
        else if x = 1 then 5
        else raise NiuNiv

    let creer () (c: coord): t =
        assert (c = (2, 0));
        {
            u = 0;
            v = 0
        }

    let nb_cases (g: t): int =
        2

    let lineariser (_: t) ((x, _): coord): int =
        x + 1

    let valeur (g: t) ((x, y): coord): int =
        if x = 0 then g.u
        else if x = 1 then g.v
        else raise NiuNiv

    let deposer (g: t) (n: int) ((x, y): coord): unit =
        assert (y = 0);
        if x = 0 then g.u <- g.u + n
        else if x = 1 then g.v <- g.v + n
        else raise NiuNiv

    let correcte_coord (g: t) ((x, y): coord): bool =
        (x = 0 || x = 1) && y = 0

    let voisins (g: t) ((x, y): coord): coord list =
        assert (y = 0);
        if x = 0 then [(1, 0); (1, 0); (1, 0); (1, 0); (1, 0)]
        else if x = 1 then [(0, 0); (0, 0); (0, 0)]
        else raise NiuNiv

    let copier (g: t): t =
        {
            u = g.u;
            v = g.v
        }

    let dimensions (g: t): coord =
        (2, 0)

    let superposer (g1: t) (g2: t): t =
        let g = creer () (2, 0) in

        g.u <- g1.u + g2.u;
        g.v <- g1.v + g2.v;

        g

    let iterer (f: coord -> unit) (g: t): unit =
        f (0, 0);
        f (1, 0)

    let imprimer (g: t): unit =
        for x = 0 to 1 do
            (x, 0) |> (valeur g) |>
            (fun n -> char_of_int (n + 48) (* char_of_int '0' *)
            ) |> print_char
        done;
        print_newline ()

    let dim_cases: int ref = ref 200 (* Taille par défaut *)

    let mettre_dim_cases (a: int): unit =
        dim_cases := a

    let couleur_case (g: t) (c: coord): Graphics.color =
        let (x, y) = c in
        match valeur g (x, 0) with
        | n -> let u = 255 - 255 * n / max_valeur g (x, 0) in
            Graphics.rgb u u u

    let ouvrir_fenetre (g: t): unit =
        " " ^ (2 * !dim_cases |> string_of_int)
        ^ "x" ^ (!dim_cases |> string_of_int)
        |> Graphics.open_graph

    let afficher_grille (g: t) (g_opt: t option): unit =
        let egal_grilles : coord -> bool = match g_opt with
        | None -> fun _ -> false
        | Some(gg) -> fun c -> (valeur g c) = (valeur gg c)
        in

        iterer
            (fun (x, y) ->
                assert (y = 0);
                if egal_grilles (x, 0) then ()
                else begin
                    couleur_case g (x, 0) |> Graphics.set_color;
                    Graphics.fill_rect
                        (!dim_cases*x)
                        (!dim_cases*y)
                        !dim_cases
                        !dim_cases
                end
            )
            g

    let afficher_case (g: t) (c: coord): unit =
        let (x, y) = c in
        Graphics.fill_rect (!dim_cases*x) (!dim_cases*y) !dim_cases !dim_cases
end

(* Tas de sable exemple *)
module Tse = Tas_sable(Grille_exemple)

