open Tas_sable

module Grille_carre: GRILLE = struct
    type t = {
        grille: int array array;
        largeur: int;
        hauteur: int;
    }

    let max_valeur = 3

    let créer (dim: coord): t =
        let (x, y) = dim in {
            grille = Array.make_matrix x y 0;
            largeur = x;
            hauteur = y
        }

    let valeur (g: t) (c: coord): int =
        let (x, y) = c in
        g.grille.(x).(y)

    let déposer (g: t) (n: int) (c: coord): unit =
        let (x, y) = c in
        g.grille.(x).(y) <- g.grille.(x).(y) + n

    let voisins (g: t) (c: coord): coord list =
        let (x, y) = c in
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
        let g = créer (g1.largeur, g1.hauteur) in

        for x = 0 to g1.largeur - 1 do
            for y = 0 to g1.hauteur - 1 do
                g.grille.(x).(y) <- g1.grille.(x).(y) + g2.grille.(x).(y)
            done
        done;

        g

    let itérer (f: coord -> unit) (g: t): unit =
        for x = 0 to g.largeur - 1 do
            for y = 0 to g.hauteur - 1 do
                f (x, y)
            done
        done

    let imprimer (g: t): unit =
        for x = 0 to g.largeur - 1 do
            for y = 0 to g.hauteur - 1 do
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

    let a: int = 10

    let ouvrir_fenêtre (g: t): unit =
        " " ^ (g.largeur * a |> string_of_int)
        ^ "x" ^ (g.hauteur * a |> string_of_int)
        |> Graphics.open_graph

    let afficher (g: t): unit =
        itérer
            (fun (x,y)  ->
                (match valeur g (x, y) with
                (*| 0 -> Graphics.rgb 255 255 255
                | 1 -> Graphics.rgb 140 140 140
                | 2 -> Graphics.rgb 70 70 70
                | 3 -> Graphics.rgb 0 0 0
                | _ -> failwith ( "La valeur de "
                    ^ (string_of_int x) ^ "," ^ (string_of_int y)
                    ^ " depasse " ^ (string_of_int max_valeur) ) (* é *)
                *)
                | n -> let u = 255 - 255 * n / max_valeur in
                Graphics.rgb u u u
                ) |>  Graphics.set_color;
                Graphics.fill_rect (a*x) (a*y) a a)
            g
end

(* Tas de sable carré *)
module Tsc = Tas_sable(Grille_carre)
