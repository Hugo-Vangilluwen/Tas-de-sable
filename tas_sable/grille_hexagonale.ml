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
        (* Effectue l'opération f x avec x vu comme un flottant *)
        let float_calcul (f: float -> float) (x: int) =
            x |> float_of_int |> f |> int_of_float
        in
        (* Trace polygone de centre c et de sommets s
         * Compense les défauts de Graphics.fill_poly
         *)
        let tracer_poly (c: int * int) (s: (int * int) array): unit =
            let n = Array.length s in
            for i = 0 to (n - 2) do
                Graphics.draw_poly [|c; s.(i); s.(i+1)|]
            done;
            for i = 0 to (n - 1) do
                let (cx, cy) = s.(i) in
                Graphics.fill_circle cx cy 5
            done
        in

        let a = 50 in
        (* cos 60 = 0.5 et sin 60 = 0.866 *)
        let b = float_calcul (( *.) 0.866) a in
        let c = float_calcul (( *.) 0.5) a in
        print_int b; print_newline ();
        print_int c; print_newline ();

        " " ^ (g.largeur * 2 * a |> string_of_int)
        ^ "x" ^ (g.hauteur * 2 * b |> string_of_int)
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
                let xx = float_calcul ((float_of_int a) |>( *.)) (2*x-y) in
                let yy = 2 * b * y in
(*                 tracer_poly *)
(*                     (xx + a, yy + b) *)
                Graphics.fill_poly
                    [|
                    (xx, yy + b);
                    (xx + c, yy + b + b);
                    (xx + a + c, yy + b + b);
                    (xx + a + a, yy + b);
                    (xx + a + c, yy);
                    (xx + c, yy);
                    |]
                (*Graphics.draw_poly [|(xx + a, yy + b); (xx + c, yy + b + b); (xx + a + c, yy + b + b)|];
                Graphics.draw_poly [|(xx + a, yy + b); (xx, yy + b); (xx + c, yy + b + b)|]*)
            )
            g;

        let _ = Graphics.wait_next_event[Key_pressed] in
        Graphics.close_graph ()
end

(* Tas de sable carré *)
module Tsh = Tas_sable(Grille_hexagonale)
