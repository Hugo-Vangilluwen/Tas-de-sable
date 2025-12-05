open Tas_sable

module Grille_montagne: GRILLE = struct
    type t = {
        grille: int array array;
        largeur: int;
        hauteur: int;
        alpha : float;
    }

    let alpha_defaut = Float.pi /. 4.0

    let creer (dim: coord) : t =
        let (x, y) = dim in
        { grille = Array.make_matrix x y 0;
            largeur = x;
            hauteur = y;
            alpha = alpha_defaut }

    let valeur (g: t) (c: coord): int =
        let (x, y) = c in
        g.grille.(x).(y)

    let deposer (g: t) (n: int) (c: coord): unit =
        let (x, y) = c in
        g.grille.(x).(y) <- g.grille.(x).(y) + n

    let correcte_coord (g: t) (c: coord): bool =
        let (x, y) = c in
        0 <= x && x < g.largeur && 0 <= y && y < g.hauteur

    let voisins (g: t) ((x, y): coord): coord list =
        let v = ref [] in
        let ajouter (c: coord) (n: int) =
            for _ = 1 to n do
                v := c :: !v
            done
        in

        let a = g.alpha in
        let k = min 1.0 (max 0.0 (a /. (Float.pi /. 2.0))) in

        let m_haut   = int_of_float (1.0 *. (1.0 -. k)) in
        let m_droite = int_of_float (1.0 *. (1.0 -. k)) in
        let m_bas    = int_of_float (1.0 *. (1.0 +. k)) in
        let m_gauche = int_of_float (1.0 *. (1.0 +. k)) in

        let m_diag =
            if a > Float.pi /. 4.0 then
                int_of_float (2.0 *. (a -. Float.pi /. 4.0) /. (Float.pi /. 4.0))
            else
                0
        in


        if y > 0 then ajouter (x, y-1) m_bas;
        if x > 0 then ajouter (x-1, y) m_gauche;
        if x > 0 && y > 0 then ajouter (x-1, y-1) m_diag;
        if y < g.hauteur - 1 then ajouter (x, y+1) m_haut;
        if x < g.largeur - 1 then ajouter (x+1, y) m_droite;

        !v


    let voisins (g: t) ((x, y): coord): coord list =
        let v = ref [] in
        let ajouter (c: int*int) (n: int) = 
            for _ = 1 to n do 
                v := c :: !v 
            done 
        in
        let alpha_diag = Float.pi /. 4.0 in
        let m_bas = max 1 (int_of_float (g.alpha *. 4.0)) in
        let m_gauche = m_bas in
        let m_haut = 1 in
        let m_droite = m_haut in

        if y > 0 then 
            ajouter (x, y-1) m_bas;
        if x > 0 then 
            ajouter (x-1, y) m_gauche;

        if g.alpha >= alpha_diag then begin
            let m_diag = int_of_float (g.alpha *. 2.0) in
            if x > 0 && y > 0 then 
                ajouter (x-1, y-1) m_diag;
        end;

        if y < g.hauteur - 1 then 
            ajouter (x, y+1) m_haut;
        if x < g.largeur - 1 then 
            ajouter (x+1, y) m_droite;

        !v

    let max_valeur g c = List.length (voisins g c) - 1

    let copier (g: t): t =
        {
            grille = Array.map Array.copy g.grille;
            largeur = g.largeur;
            hauteur = g.hauteur;
            alpha = g.alpha
        }

    let dimensions (g: t): coord =
        (g.largeur, g.hauteur)

    let superposer (g1: t) (g2: t): t =
        assert (g1.largeur = g2.largeur && g1.hauteur = g2.hauteur && g1.alpha = g2.alpha);
        let g = { 
            grille = Array.make_matrix g1.largeur g1.hauteur 0;
            largeur = g1.largeur;
            hauteur = g1.hauteur;
            alpha = g1.alpha
        } in

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
        | Some(gg) -> fun c -> (valeur g c) == (valeur gg c)
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

(* Tas de sable montagne *)
module Tsm = Tas_sable(Grille_montagne)
