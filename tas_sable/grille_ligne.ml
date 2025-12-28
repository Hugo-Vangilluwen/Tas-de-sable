(* Modélise une grille en ligne
 *)

open Tas_sable

module Grille_ligne: GRILLE with type param = unit = struct
    type param = unit

    type t = {
        ligne: int array;
        longueur: int;
    }

    let max_valeur (_: t) (_: coord): int = 1

    let creer () ((x, y): coord): t =
        assert (y = 0);
        {
            ligne = Array.make x 0;
            longueur = x;
        }

    let valeur (g: t) ((x, y): coord): int =
        g.ligne.(x)

    let deposer (g: t) (n: int) ((x, y): coord): unit =
        assert (y = 0);
        g.ligne.(x) <- g.ligne.(x) + n

    let correcte_coord (g: t) ((x, y): coord): bool =
        0 <= x && x < g.longueur && y = 0

    let voisins (g: t) ((x, y): coord): coord list =
        assert (y = 0);
        let v = ref [] in

        if 0 < x then
            v := (x-1, y) :: !v
        else ();
        if x < g.longueur - 1 then
            v := (x+1, y) :: !v
        else ();

        !v

    let copier (g: t): t =
        {
            ligne = Array.copy g.ligne;
            longueur = g.longueur
        }

    let dimensions (g: t): coord =
        (g.longueur, 0)

    let superposer (g1: t) (g2: t): t =
        assert (g1.longueur = g2.longueur);
        let g = creer () (g1.longueur, 0) in

        for x = 0 to g1.longueur - 1 do
            g.ligne.(x) <- g1.ligne.(x) + g2.ligne.(x)
        done;

        g

    let iterer (f: coord -> unit) (g: t): unit =
        for x = 0 to g.longueur - 1 do
            f (x, 0)
        done

    let imprimer (g: t): unit =
        for x = 0 to g.longueur - 1 do
            (x, 0) |> (valeur g) |>
            (fun n -> match n with
            | 0 -> ' '
            | 1 -> '*'
            | _ -> char_of_int (n + 48) (* char_of_int '0' *)
            ) |> print_char
        done;
        print_newline ()

    let dim_cases: int ref = ref 50 (* Taille par défaut *)

    let mettre_dim_cases (a: int): unit =
        dim_cases := a

    let couleur_case (g: t) (c: coord): Graphics.color =
        let (x, y) = c in
        match valeur g (x, 0) with
        | n -> let u = 255 - 255 * n / max_valeur g (x, 0) in
            Graphics.rgb u u u

    let ouvrir_fenetre (g: t): unit =
        " " ^(g.longueur * !dim_cases |> string_of_int)
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

(* Tas de sable linéaire *)
module Tsl = Tas_sable(Grille_ligne)
