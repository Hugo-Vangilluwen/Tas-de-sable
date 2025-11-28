(* Modelise un grille carree
 *)

open Tas_sable

module Grille_carree: GRILLE = struct
    type t = {
        grille: int array array;
        largeur: int;
        hauteur: int;
    }

    let max_valeur (_: t) (_: coord): int = 3

    let creer (dim: coord): t =
        let (x, y) = dim in {
            grille = Array.make_matrix x y 0;
            largeur = x;
            hauteur = y
        }

    let valeur (g: t) ((x, y): coord): int =
        g.grille.(x).(y)

    let deposer (g: t) (n: int) ((x, y): coord): unit =
        g.grille.(x).(y) <- g.grille.(x).(y) + n

    let correcte_coord (g: t) ((x, y): coord): bool =
        0 <= x && x < g.largeur && 0 <= y && y < g.hauteur

    let voisins (g: t) ((x, y): coord): coord list =
        let v = ref [] in

        if 0 < x then
            v := (x-1, y) :: !v
        else ();
        if x < g.largeur - 1 then
            v := (x+1, y) :: !v
        else ();
        if 0 < y then
            v := (x, y-1) :: !v
        else ();
        if y < g.hauteur - 1 then
            v := (x, y+1) :: !v
        else ();

        !v

    let copier (g: t): t =
        {
            grille = Array.map Array.copy g.grille;
            largeur = g.largeur;
            hauteur = g.hauteur
        }

    let dimensions (g: t): coord =
        (g.largeur, g.hauteur)

    let superposer (g1: t) (g2: t): t =
        assert (g1.largeur = g2.largeur && g1.hauteur = g2.hauteur);
        let g = creer (g1.largeur, g1.hauteur) in

        for x = 0 to g1.largeur - 1 do
            for y = 0 to g1.hauteur - 1 do
                g.grille.(x).(y) <- g1.grille.(x).(y) + g2.grille.(x).(y)
            done
        done;

        g

    let iterer (f: coord -> unit) (g: t): unit =
        for x = 0 to g.largeur - 1 do
            for y = 0 to g.hauteur - 1 do
                f (x, y)
            done
        done

    (*
    exception Existence
    let existe (f: coord -> bool) (g: t): bool =
        let existe_aux (): unit =
            for x = 0 to g.largeur - 1 do
                for y = 0 to g.hauteur - 1 do
                    if f (x, y) then
                        raise Existence
                    else ();
                done
            done
        in
        try (let _ = existe_aux () in false) with
        | Existence -> true
    *)

    let imprimer (g: t): unit =
        for y = g.hauteur - 1 downto 0 do
            for x = 0 to g.largeur - 1 do
                (x, y) |> (valeur g) |>
                (fun n -> match n with
                | 0 -> ' '
                | 1 -> '.'
                | 2 -> '*'
                | 3 -> '0'
                | _ -> char_of_int (n + 48) (* char_of_int '0' *)
                ) |> print_char
            done;
            print_newline ()
        done

    let a: int = 20

    let ouvrir_fenetre (g: t): unit =
        " " ^ (g.largeur * a |> string_of_int)
        ^ "x" ^ (g.hauteur * a |> string_of_int)
        |> Graphics.open_graph

    let afficher_grille (g: t) (g_opt: t option): unit =
        let egal_grilles : coord -> bool = match g_opt with
        | None -> fun _ -> false
        | Some(gg) -> fun c -> (valeur g c) = (valeur gg c)
        in

        iterer
            (fun (x,y)  ->
                if egal_grilles (x, y) then ()
                else begin
                    (match valeur g (x, y) with
                    | n -> let u = 255 - 255 * n / max_valeur g (x, y) in
                    Graphics.rgb u u u
                    ) |>  Graphics.set_color;
                    Graphics.fill_rect (a*x) (a*y) a a
                end
            )
            g
end

(* Tas de sable carre *)
module Tsc = Tas_sable(Grille_carree)
