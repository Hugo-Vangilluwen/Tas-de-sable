open Tas_sable

module Grille_montagne : GRILLE with type param = float = struct
    type param = float

    type t = {
        grille: int array array;
        largeur: int;
        hauteur: int;
        alpha : float;
    }

    (* alpha est en radian *)
    let creer (alpha: param) (dim: coord) : t =
        let (x, y) = dim in
        { grille = Array.make_matrix x y 0;
            largeur = x;
            hauteur = y;
            alpha = alpha }

    let nb_cases (g: t): int =
        g.largeur * g.hauteur

    let lineariser (g: t) ((x, y): coord): int =
        x * g.hauteur + y + 1

    let valeur (g: t) (c: coord): int =
        let (x, y) = c in
        g.grille.(x).(y)

    let deposer (g: t) (n: int) (c: coord): unit =
        let (x, y) = c in
        g.grille.(x).(y) <- g.grille.(x).(y) + n

    let correcte_coord (g: t) (c: coord): bool =
        let (x, y) = c in
        0 <= x && x < g.largeur && 0 <= y && y < g.hauteur

    let voisins (g: t) ((x, y): coord) : coord list =
        let v = ref [] in

        let alpha = max 0.0 (min (Float.pi /. 2.0) g.alpha) in

        let denom = exp ((Float.pi /. 2.0)) in
        let k = if alpha = 0.0 then 0.0
            else exp (alpha) /. denom in


        let w_side = (-. 110.0 /. 21.0) *. (k *. k) +. (89.0 /. 21.0) *. k +. 1.
        and w_diag = k
        and w_opposite = (exp(-. 10.0 *. k) -. exp(-. 10.))/.(1. -. exp(-. 10.)) in

        let candidats = [
            ((0,-1), w_side);
            ((-1,0), w_side);
            ((-1,-1), w_diag);
            ((0,1), w_opposite);
            ((1,0), w_opposite)
        ] in

        let l = List.filter (fun ((dx,dy),_) ->
            let nx = x + dx and ny = y + dy in
            0 <= nx && nx < g.largeur && 0 <= ny && ny < g.hauteur
        ) candidats in

        let n_tot = 12 in

        let wtot = List.fold_left (fun s (_, w) -> s +. w) 0.0 l |> max 1e-6 in

        List.iter (fun ((dx,dy), w) ->
            let n_c = float n_tot *. (w /. wtot) in

            let n =
                if alpha = 0.0 then int_of_float n_c
                else max 1 (int_of_float n_c)
            in

            let nx = x + dx and ny = y + dy in
            for _ = 1 to n do v := (nx, ny) :: !v done
        ) l;

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

    let dim_cases: int ref = ref 20 (* Taille par défaut *)

    let mettre_dim_cases (a: int): unit =
        dim_cases := a

    let couleur_case (g: t) (c: coord): Graphics.color =
        let (x, y) = c in
        match valeur g (x, y) with
        | n -> let u = 255 - 255 * n / max_valeur g (x, y) in
            Graphics.rgb u u u

    let ouvrir_fenetre (g: t): unit =
        " " ^ (g.largeur * !dim_cases |> string_of_int)
        ^ "x" ^ (g.hauteur * !dim_cases |> string_of_int)
        |> Graphics.open_graph

    (*
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
    *)

    let afficher_case (g: t) (c: coord): unit =
        let (x, y) = c in
        Graphics.fill_rect (!dim_cases*x) (!dim_cases*y) !dim_cases !dim_cases
end

(* Tas de sable montagne *)
module Tsm = Tas_sable(Grille_montagne)
