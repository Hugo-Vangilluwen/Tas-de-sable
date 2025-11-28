(* Type représentant les coordonnées d'une case *)
type coord = int * int

(* Signature d'une structure de grille *)
module type GRILLE = sig
    (* Type représentant une grille *)
    type t

    (* Nombre maximum de voisin moins un
     * c'est-à-dire la valeur maximal de la case
     *)
    val max_valeur : t -> coord -> int

    (* Crée une grille de dimension n x m *)
    val créer : coord -> t

    (* Renvoie la valeur de la case de coordonnées c *)
    val valeur : t -> coord -> int

    (* Modifie la valeur de la case de coordonnées c *)
    val déposer : t -> int -> coord -> unit

    (* Teste si la coordonnée est correcte *)
    val correcte_coord : t -> coord -> bool

    (* Renvoie les voisins de c *)
    val voisins : t -> coord -> coord list

    (* Copie une grille *)
    val copier : t -> t

    (* Retourne les dimensions de la grille *)
    val dimensions : t -> coord

    (* Superpose deux tas de sable *)
    val superposer : t -> t -> t

    (* Itère la fonction f parmis les cases de g *)
    val itérer : (coord -> unit) -> t -> unit

(*     val existe : (coord -> bool) -> t -> bool *)

    (* Imprime la grille dans la console *)
    val imprimer : t -> unit

    (* Ouvre une fenêtre Graphics de la bonne taille *)
    val ouvrir_fenêtre : t -> unit

    (* Affiche la grille dans une fenêtre graphique
     * Les valeurs de la grille doivent être entre 0 et max_voisin - 1
     * Si une seconde grille est donnée, seuls les cases de valeurs différentes
     * sont redessinées
     *)
    val afficher_grille : t -> t option -> unit
end


(* Modélise un tas de sable abélien *)
module Tas_sable (G: GRILLE) = struct
    include G

    (* Renvoie un string représentant c *)
    let coord_en_string (c: coord): string =
        let (x, y) = c in
        (string_of_int x) ^ "," ^ (string_of_int y)

    (* Lève une erreur si c n'est pas correcte dans tas *)
    let tester_coord (tas: t) (c: coord): unit =
        if correcte_coord tas c then ()
        else failwith ("La coordonnée ("
            ^ (coord_en_string c)
            ^") n'est pas correcte")

    (* Calcule un glissement de tas
     * Vaut vrai si aucune glissement a lieu et faux sinon
     *)
    let glissement (tas: t): bool =
        let glissé = ref false in

        itérer
            (fun (c: coord): unit ->
                if max_valeur tas c < valeur tas c then begin
                    glissé := true;
                    déposer tas (- max_valeur tas c - 1) c;
                    List.iter
                        (déposer tas 1)
                        (voisins tas c)
                end else ()
            )
            tas;

        if !glissé then
            true
        else
            false

    (* Calcule tous les glissements jusqu'à que le tas de sable soit stable *)
    let rec avalanche (tas: t): unit =
        if glissement tas then
            avalanche tas
        else ()

    (* Somme tas1 et tas2 *)
    let (+) (tas1: t) (tas2: t) =
        let somme = superposer tas1 tas2 in
        avalanche somme;
        somme

    (* Affiche le tas de sable dans une fenêtre graphique *)
    let afficher (tas: t): unit =
        ouvrir_fenêtre tas;
        afficher_grille tas None;

        let _ = Graphics.wait_next_event[Button_down] in ();
        Graphics.close_graph ()

    (* Affiche l'animation de n étape à partir de tas en ajoutant la source à
     * chaque étape en passant d'une étape à une autre avec attendre
     * Renvoie le tas final
     *)
    let animer (tas: t) (n :int) (source: t) (attendre: unit -> unit): t =
        ouvrir_fenêtre tas;
        afficher_grille tas None;

        let tas_animé = ref tas in

        for i = 1 to n do
            let tas_tmp = !tas_animé + source in
            afficher_grille tas_tmp (Some(!tas_animé));
            tas_animé := tas_tmp;
            attendre ()
        done;

        let _ = Graphics.wait_next_event[Key_pressed] in ();

        Graphics.close_graph ();
        !tas_animé

    (* Dépose un à un les grain de sable dans tas dans la case c
     * change d'étape à chaque appuis de touche sur le clavier
     * Effectue au total n étapes
     *)
    let un_grain_clavier (tas: t) (c: coord) (n: int): t =
        let source = tas |> dimensions |> créer in
        déposer source 1 c;
        let attendre () =
            let _ = Graphics.wait_next_event[Key_pressed] in ()
        in
        animer tas n source attendre

    (* Dépose un à un les grain de sable dans tas dans la case c
     * attend dt secondes entre chaque étape
     * Effectue au total n étapes
     *)
    let un_grain_temps (tas: t) (c: coord) (n: int) (dt: float): t =
        let source = tas |> dimensions |> créer in
        déposer source 1 c;
        let attendre () =
            Unix.sleepf dt
        in
        animer tas n source attendre

    (* Calcule l'identité du groupe
     * des tas de sables récurrents de dimensions dim
     * en utilisant la formule : (2c_max - (2c_max)°)°
     *)
    let identité (dim: coord): t =
        let double_max = créer dim in
        itérer
            (fun c -> déposer double_max (2 * max_valeur double_max c) c)
            double_max;

        let stat_db_max = copier double_max in
        avalanche stat_db_max;

        (* Multiplie par -1 *)
        itérer
            (fun c -> déposer stat_db_max (-2 * (valeur stat_db_max c)) c)
            stat_db_max;

        double_max + stat_db_max


end
