open Tas_sable

module Grille_hexagonale: GRILLE = struct
    type t = {
        grille: int array array;
        largeur: int;
        hauteur: int;
    }

    let max_valeur = 5

    let créer (c: coord): t =
        let (x, y) = c in {
            grille = Array.make_matrix (x+y-1) y 0;
            largeur = x+y-1;
            hauteur = y
        }

    (* Teste si la coordonnée est correcte dans g *)
    let correcte_coord (g: t) (c: coord): bool =
        let (x, y) = c in
        0 <= y && y <= x && x <= g.largeur + y - 1 && y <= g.hauteur

    let tester_coord (g: t) (c: coord): unit =
        if correcte_coord g c then ()
        else let (x, y) = c in
            failwith ("La coordonnée ("
            ^ (string_of_int x) ^ "," ^ (string_of_int y)
            ^") n'est pas correcte")

    let valeur (g: t) (c: coord): int =
        tester_coord g c;
        let (x, y) = c in
        g.grille.(x).(y)

    let modifier (g: t) (n: int) (c: coord): unit =
        tester_coord g c;
        let (x, y) = c in
        g.grille.(x).(y) <- g.grille.(x).(y) + n

    let voisins (g: t) (c: coord): coord list =
        tester_coord g c;
        let (x, y) = c in
        let v = ref [] in

        if y < x then
            v := (x-1, y) :: !v
        else ();
        if x < g.largeur + y - 1 then
            v := (x+1, y) :: !v
        else ();
        if 0 < y then begin
            if x <= g.largeur + y - 1 then
                v := (x, y-1) :: !v
            else ();
            if x < g.largeur + y - 1 then
                v := (x-1, y-1) :: !v
            else ()
        end else ();
        if y < g.hauteur - 1 then begin
            if y < x && x <= g.largeur + y then
                v := (x, y+1) :: !v
            else ();
            if y <= x then
                v := (x+1, y+1) :: !v
            else ()
        end else ();

        !v

    let copier (g: t): t =
        {
            grille = Array.map Array.copy g.grille;
            largeur = g.largeur;
            hauteur = g.hauteur
        }

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
                if (x, y) |> correcte_coord g then
                    f (x, y)
                else ()
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

    let afficher (g: t): unit =
        let a = 30 in
        (* cos 60 = 0.5 et sin 60 = 0.866 *)
        let b = a |> float_of_int |> ( *. ) 0.866 |> int_of_float in
        let c = a |> float_of_int |> ( *. ) 0.5 |> int_of_float in
        print_int b; print_newline ();
        print_int c; print_newline ();

        " " ^ (g.largeur * a |> string_of_int)
        ^ "x" ^ (g.hauteur * a |> string_of_int)
        |> Graphics.open_graph;

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
                    ^ ") dépasse " ^ (string_of_int max_valeur) )
                ) |>  Graphics.set_color;
                let xx = a * x and yy = a * y in
                Graphics.draw_poly [|
                    (xx, yy + b);
                    (xx + c, yy + b + b);
                    (xx + a + c, yy + b + b);
                    (xx + a + a, yy + b);
                    (xx + a + c, yy);
                    (xx + c, yy);
                |])
            g;

        let _ = Graphics.wait_next_event[Key_pressed] in
        Graphics.close_graph ()
end

(* Tas de sable carré *)
module Tsh = Tas_sable(Grille_hexagonale)
