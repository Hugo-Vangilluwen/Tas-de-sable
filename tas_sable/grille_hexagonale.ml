open Tas_sable

module Grille_hexagonale: GRILLE = struct
    type t = {
        grille: int array array;
        largeur: int;
        hauteur: int;
    }

    let max_valeur (_: coord): int = 5

    let créer (c: coord): t =
        let (x, y) = c in {
            grille = Array.make_matrix x y 0;
            largeur = x;
            hauteur = y
        }

    let correcte_coord (g: t) (c: coord): bool =
        let (x, y) = c in
        0 <= x && x < g.largeur && 0 <= y && y < g.hauteur

    let valeur (g: t) (c: coord): int =
        let (x, y) = c in
        g.grille.(x).(y)

    let déposer (g: t) (n: int) (c: coord): unit =
        let (x, y) = c in
        g.grille.(x).(y) <- g.grille.(x).(y) + n

    let voisins (g: t) (c: coord): coord list =
        let (x, y) = c in
        let v = ref [] in

        if 0 < x then begin
            v := (x-1, y) :: !v;
            if y < g.hauteur - 1 then
                v := (x-1, y+1) :: !v
            else ()
        end else ();
        if x < g.largeur - 1 then begin
            v := (x+1, y) :: !v;
            if 0 < y then
                v := (x+1, y-1) :: !v
            else ()
        end else ();
        if 0 < y then
            v := (x, y-1) :: !v
        else ();
        if y < g.hauteur - 1 then
            v := (x, y+1) :: !v
        else ();

        !v

        (*
        List.filter
            (correcte_coord g)
            [ (x-1, y); (x+1, y); (x, y-1); (x+1, y-1); (x, y+1); (x-1,y+1) ]
        *)

    let voisins_potentiels (g: t) (c: coord): coord list =
        let (x, y) = c in

        (*List.filter
            (correcte_coord g)*)
            [ (x-1, y); (x+1, y); (x, y-1); (x+1, y-1); (x, y+1); (x-1,y+1) ]

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
        let g =  g1 |> dimensions |> créer in

        for x = 0 to g.largeur - 1 do
            for y = 0 to g.hauteur - 1 do
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
                if (x, y) |> correcte_coord g then
                    (x, y) |> (valeur g) |>
                    (fun n -> match n with
                    | 0 -> ' '
                    | 1 -> '.'
                    | 2 -> ':'
                    | 3 -> '*'
                    | 4 -> '#'
                    | 5 -> '0'
                    | _ -> char_of_int (n + 48) (* char_of_int '0' *)
                    ) |> print_char
                else print_char ' '
            done;
            print_newline ()
        done

    (* Effectue l'opération f x avec x vu comme un flottant *)
    let float_calcul (f: float -> float) (x: int) =
        x |> float_of_int |> f |> int_of_float

    let a : int = 10
    (* cos 30 = 0,866 et sin 30 = 0,5 *)
    let b : int = float_calcul (( *.) 0.866) a (* largeur de l'hexagone *)
    let c : int = float_calcul (( *.) 0.5) a (* demi-longeur d'un coté *)

    let ouvrir_fenêtre (g: t): unit =
        " " ^ ((2 * g.largeur + g.hauteur - 1) * b |> string_of_int)
        ^ "x" ^ (g.hauteur * (a + c) + (a - c) |> string_of_int)
        |> Graphics.open_graph

    let afficher (g: t): unit =
        itérer
            (fun (x,y)  ->
                (match valeur g (x, y) with
                | 0 -> Graphics.rgb 255 255 255
                | 1 -> Graphics.rgb 200 200 200
                | 2 -> Graphics.rgb 150 150 150
                | 3 -> Graphics.rgb 100 100 100
                | 4 -> Graphics.rgb 50 50 50
                | 5 -> Graphics.rgb 0 0 0
                | _ -> failwith ( "La valeur de ("
                    ^ (string_of_int x) ^ "," ^ (string_of_int y)
                    ^ ") dépasse " ^ ((x, y) |> max_valeur |> string_of_int) )
                ) |>  Graphics.set_color;
                let xx = b * (2 * x + y) in
                let yy = (a + c) * y in
                Graphics.fill_poly
                    [|
                    (xx + b, yy);
                    (xx + b + b, yy + a - c);
                    (xx + b + b, yy + a + c);
                    (xx + b, yy + a + a);
                    (xx, yy + a + c);
                    (xx, yy + a - c);
                    |]
            )
            g
end

(* Tas de sable carré *)
module Tsh = Tas_sable(Grille_hexagonale)
