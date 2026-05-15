(* Modélise une grille d'un graphe complet
 *)

open Tas_sable

module Grille_complete: GRILLE with type param = unit = struct
    type param = unit

    type t = {
        sommets: int array;
        nombre: int;
    }

    let max_valeur (g: t) (_: coord): int =
        g.nombre - 1

    let creer () ((x, y): coord): t =
        if (y = 0)  then ()
        else raise (Invalid_argument "La variable y doit être nulle");

        {
            sommets = Array.make x 0;
            nombre = x;
        }

    let nb_cases (g: t): int =
        g.nombre

    let lineariser (g: t) ((x, y): coord): int =
        x + 1

    let valeur (g: t) ((x, y): coord): int =
        g.sommets.(x)

    let deposer (g: t) (n: int) ((x, y): coord): unit =
        if (y = 0)  then ()
        else raise (Invalid_argument "La variable y doit être nulle");

        g.sommets.(x) <- g.sommets.(x) + n

    let correcte_coord (g: t) ((x, y): coord): bool =
        0 <= x && x < g.nombre && y = 0

    let voisins (g: t) ((x, y): coord): coord list =
        List.init
            (g.nombre - 1)
            (fun i -> ((if i < x then i else i+1), 0) )

    let copier (g: t): t =
        {
            sommets = Array.copy g.sommets;
            nombre = g.nombre
        }

    let dimensions (g: t): coord =
        (g.nombre, 0)

    let superposer (g1: t) (g2: t): t =
        assert (g1.nombre = g2.nombre);
        let g = creer () (g1.nombre, 0) in

        for x = 0 to g1.nombre - 1 do
            g.sommets.(x) <-
                g1.sommets.(x) + g2.sommets.(x)
        done;

        g

    let iterer (f: coord -> unit) (g: t): unit =
        for x = 0 to g.nombre - 1 do
            f (x, 0)
        done

    let imprimer (g: t): unit =
        for x = 0 to g.nombre - 1 do
            (x, 0) |> (valeur g) |>
            (fun n -> char_of_int (n + 48) (* char_of_int '0' *)
            ) |> print_char
        done;
        print_newline ()

    let taille_cases: int ref = ref 50 (* Taille par défaut *)

    let obtenir_taille_cases (): int =
        !taille_cases

    let mettre_taille_cases (a: int): unit =
        taille_cases := a

    let couleur_case (g: t) (c: coord): Graphics.color =
        let (x, y) = c in
        match valeur g (x, 0) with
        | n -> let u = 255 - 255 * n / max_valeur g (x, 0) in
            Graphics.rgb u u u

    let taille_fenetre (g: t): int * int =
        g.nombre * !taille_cases, !taille_cases

    let afficher_grille (g: t) (g_opt: t option): unit =
        let egal_grilles : coord -> bool = match g_opt with
        | None -> fun _ -> false
        | Some(gg) -> fun c -> (valeur g c) = (valeur gg c)
        in

        iterer
            (fun (x, y) ->
                if (y = 0)  then ()
                else raise (Invalid_argument "La variable y doit être nulle");

                if egal_grilles (x, 0) then ()
                else begin
                    couleur_case g (x, 0) |> Graphics.set_color;
                    Graphics.fill_rect
                        (!taille_cases*x)
                        (!taille_cases*y)
                        !taille_cases
                        !taille_cases
                end
            )
            g

    let afficher_case (g: t) (c: coord): unit =
        let (x, y) = c in
        Graphics.fill_rect (!taille_cases*x) (!taille_cases*y) !taille_cases !taille_cases
end

(* Tas de sable complet *)
module Tscomplet = Tas_sable(Grille_complete)
