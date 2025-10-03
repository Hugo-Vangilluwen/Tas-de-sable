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
    val modifier : t -> int -> coord -> unit
    (* Renvoie les voisins *)
    val voisins : t -> coord -> coord list
    (* Copie une grille *)
    val copier  : t -> t
    (* Superpose deux tas de sable *)
    val superposer : t -> t -> t
    (* Itère la fonction f parmis les cases de g *)
    val itérer : (coord -> unit) -> t -> unit
    (* Imprime la grille dans la console *)
    val imprimer : t -> unit
    (* Affiche la grille dans une fenêtre graphique
     * Les valeurs de la grille doivent être entre 0 et max_voisin - 1
     *)
    val afficher : t -> unit
end

(* Modélise un tas de sable abélien *)
module Tas_sable =
    functor (G: GRILLE) ->
        struct
            include G
            (* Calcule un glissement de tas
             * Vaut None si aucune glissement n'a lieu
             *)
            let glissement (tas: t): t option =
                let glissé = ref false in
                let tas_glissé = copier tas in

                itérer
                    (fun (c: coord): unit ->
                        if max_valeur < valeur tas c then begin
                            glissé := true;
                            modifier tas_glissé (- max_valeur - 1) c;
                            List.iter
                                (modifier tas_glissé 1)
                                (voisins tas c)
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
            let rec avalanche (tas: t): t =
                match glissement tas with
                | None -> tas
                | Some(tas_glissé) -> avalanche tas_glissé

            let (+) (tas1: t) (tas2: t) =
                superposer tas1 tas2 |> avalanche
        end
