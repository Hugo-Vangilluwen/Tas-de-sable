(* Type représentant les coordonnées d'une case *)
type coord = int * int

(* Signature d'une structure de grille *)
module type GRILLE = sig
    (* Type représentant une grille *)
    type t
    (* Nombre maximum de voisin *)
    val max_valeur : int
    (* Crée une grille de dimension n x m *)
    val créer : coord -> t
    (* Renvoie la valeur de la case de coordonnées c *)
    val valeur : t -> coord -> int
    (* Modifie la valeur de la case de coordonnées c *)
    val déposer : t -> int -> coord -> unit
    (* Renvoie les voisins *)
    val voisins : t -> coord -> coord list
    (* Copie une grille *)
    val copier : t -> t
    (* Retourne les dimensions de la grille *)
    val dimensions : t -> coord
    (* Superpose deux tas de sable *)
    val superposer : t -> t -> t
    (* Itère la fonction f parmis les cases de g *)
    val itérer : (coord -> unit) -> t -> unit
    (* Imprime la grille dans la console *)
    val imprimer : t -> unit
    (* Ouvre une fenêtre Graphics de la bonne taille
    *)
    val ouvrir_fenêtre : t -> unit
    (* Affiche la grille dans une fenêtre graphique
     * Les valeurs de la grille doivent être entre 0 et max_voisin - 1
     *)
    val afficher : t -> unit
end

(* Modélise un tas de sable abélien *)
module Tas_sable =
    functor (G: GRILLE) ->
        struct
            (* Calcule un glissement de tas
             * Vaut None si aucune glissement n'a lieu
             *)
            let glissement (tas: G.t): G.t option =
                let glissé = ref false in
                let tas_glissé = G.copier tas in

                G.itérer
                    (fun (c: coord): unit ->
                        if G.max_valeur < G.valeur tas c then begin
                            glissé := true;
                            G.déposer tas_glissé (- G.max_valeur - 1) c;
                            List.iter
                                (G.déposer tas_glissé 1)
                                (G.voisins tas c)
                        end else ()
                    )
                    tas;

                if !glissé then
                    Some(tas_glissé)
                else
                    None

            (* Calcule tous les glissements
             *jusqu'à que le tas de sable soit stable
             *)
            let rec avalanche (tas: G.t): G.t =
                match glissement tas with
                | None -> tas
                | Some(tas_glissé) -> avalanche tas_glissé

            let (+) (tas1: G.t) (tas2: G.t) =
                G.superposer tas1 tas2 |> avalanche


            (* Imprime le tas dans la console *)
            let imprimer (tas: G.t): unit =
                G.imprimer tas

            (* Affiche le tas de sable dans une fenêtre graphique *)
            let afficher (tas: G.t): unit =
                G.ouvrir_fenêtre tas;

                G.afficher tas;

                let _ = Graphics.wait_next_event[Key_pressed] in ();

                Graphics.close_graph ()

            (* Affiche l'animation
             * en ajoutant la source à chaque étape
             * en passant d'une étape à une autre avec attendre
            *)
            let animer
                (tas: G.t)
                (n :int)
                (source: G.t)
                (attendre: unit -> unit)
                : G.t =
                G.ouvrir_fenêtre tas;

                let tas_animé = ref tas in

                for i = 1 to n do
                    tas_animé := !tas_animé + source;
                    Graphics.clear_graph ();
                    G.afficher !tas_animé;
                    attendre ()
                done;

                let _ = Graphics.wait_next_event[Key_pressed] in ();

                Graphics.close_graph ();
                !tas_animé

            let un_grain_clavier (tas: G.t) (c: coord) (n: int): G.t =
                let source = tas |> G.dimensions |> G.créer in
                G.déposer source 1 c;
                let attendre () =
                    let _ = Graphics.wait_next_event[Key_pressed] in ()
                in
                animer
                    tas
                    n
                    source
                    attendre

            let un_grain_temps (tas: G.t) (c: coord) (n: int): G.t =
                let source = tas |> G.dimensions |> G.créer in
                G.déposer source 1 c;
                let attendre () =
                    Unix.sleepf 0.1
                in
                animer
                    tas
                    n
                    source
                    attendre

            let identité (dim: coord): G.t =
(*                 let max_grain = Int.add G.max_valeur 1 in *)
                let max_grain = G.max_valeur in

                let double_max = G.créer dim in
                G.itérer
                    (fun c -> G.déposer double_max (2 * max_grain) c)
                    double_max;

                let stat_db_max = avalanche double_max in

                G.itérer
                    (fun c -> G.déposer stat_db_max (-2 * (G.valeur stat_db_max c)) c)
                    stat_db_max;

                (*|> itérer (fun c -> déposer tas (2 * max_valeur) c)
                |> avalanche*)
                double_max + stat_db_max


        end
