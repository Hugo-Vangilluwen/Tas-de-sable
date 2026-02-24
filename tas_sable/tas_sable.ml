open EltsQ

(* Type representant les coordonnées d'une case *)
type coord = int * int

(* Signature d'une structure de grille *)
module type GRILLE = sig
    (* Paramètre interne *)
    type param

    (* Type représentant une grille *)
    type t

    (* Nombre maximum de voisin moins un
     * c'est-à-dire la valeur maximal de la case
     *)
    val max_valeur : t -> coord -> int

    (* Cree une grille de dimension n x m *)
    val creer : param -> coord -> t

    (* Renvoie le nombre de case dans la grille *)
    val nb_cases: t -> int

    (* Linéraise les coordonnées entre 1 et nb_cases grille *)
    val lineariser: t -> coord -> int

    (* Renvoie la valeur de la case de coordonnées c *)
    val valeur : t -> coord -> int

    (* Modifie la valeur de la case de coordonnées c *)
    val deposer : t -> int -> coord -> unit

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
    val iterer : (coord -> unit) -> t -> unit

(*     val existe : (coord -> bool) -> t -> bool *)

    (* Imprime la grille dans la console *)
    val imprimer : t -> unit

    (* Définit la dimension des cases dans l'affichage  *)
    val mettre_dim_cases : int -> unit

    (* Donne la couleur de la case pour l'affichage *)
    val couleur_case: t -> coord -> Graphics.color

    (* Ouvre une fenetre Graphics de la bonne taille *)
    val ouvrir_fenetre : t -> unit

    (* Affiche la grille dans une fenetre graphique
     * Les valeurs de la grille doivent etre entre 0 et max_voisin - 1
     *)
    val afficher_case: t -> coord -> unit
end


(* Modelise un tas de sable abelien *)
module Tas_sable (G: GRILLE) = struct
    include G

    (* Renvoie un string representant c *)
    let coord_en_string (c: coord): string =
        let (x, y) = c in
        (string_of_int x) ^ "," ^ (string_of_int y)

    (* Lève une erreur si c n'est pas correcte dans tas *)
    let tester_coord (tas: t) (c: coord): unit =
        if correcte_coord tas c then ()
        else failwith ("La coordonnee ("
            ^ (coord_en_string c)
            ^") n'est pas correcte")

    (* Calcule un glissement de tas
     * Vaut vrai si aucune glissement a lieu et faux sinon
     *)
    let glissement (tas: t): bool =
        let glisse = ref false in

        iterer
            (fun (c: coord): unit ->
                if max_valeur tas c < valeur tas c then begin
                    glisse := true;
                    deposer tas (- max_valeur tas c - 1) c;
                    List.iter
                        (deposer tas 1)
                        (voisins tas c)
                end else ()
            )
            tas;

        if !glisse then
            true
        else
            false

    (* Calcule tous les glissements jusqu'à que le tas de sable soit stable *)
    let rec avalanche (tas: t): unit =
        if glissement tas then
            avalanche tas
        else ()

    (* Somme tas1 et tas2 *)
    let sommer (tas1: t) (tas2: t) =
        let somme = superposer tas1 tas2 in
        avalanche somme;
        somme

    (* Calcule f( f( ... f(acc, (0, 0)) (0, 1)) ... )
     * f doit être associative et commutative
     *)
    let reduire (tas: t) (f: 'a -> coord -> 'a) (acc: 'a): 'a =
        let res = ref acc in
        iterer
            (fun c -> res := f !res c)
            tas;
        !res

    (* Affiche la grille dans une fenetre graphique
     * Les valeurs de la grille doivent etre entre 0 et max_voisin - 1
     * Si une seconde grille est donnee, seuls les cases de valeurs differentes
     * sont redessinees
     *)
    let afficher_grille (g: t) (g_opt: t option): unit =
        let egal_grilles : coord -> bool = match g_opt with
        | None -> fun _ -> false
        | Some(gg) -> fun c -> (valeur g c) = (valeur gg c)
        in

        iterer
            (fun c ->
                if egal_grilles c then ()
                else begin
                    (couleur_case g c) |> Graphics.set_color;
                    afficher_case g c
                end
            )
            g

    (* Affiche le tas de sable dans une fenetre graphique *)
    let afficher (tas: t): unit =
        ouvrir_fenetre tas;
        afficher_grille tas None;

        let _ = Graphics.wait_next_event[Button_down] in ();
        Graphics.close_graph ()

    (* Affiche l'animation de n etape à partir de tas en ajoutant la source à
     * chaque etape en passant d'une etape à une autre avec attendre
     * Renvoie le tas final
     *)
    let animer (tas: t) (n :int) (source: t) (attendre: unit -> unit): t =
        ouvrir_fenetre tas;
        afficher_grille tas None;

        let tas_anime = ref tas in

        for i = 1 to n do
            let tas_tmp = sommer !tas_anime source in
            afficher_grille tas_tmp (Some(!tas_anime));
            tas_anime := tas_tmp;
            attendre ()
        done;

        let _ = Graphics.wait_next_event[Key_pressed] in ();

        Graphics.close_graph ();
        !tas_anime

    (* Dépose un à un les grain de sable dans tas dans la case c
     * change d'étape à chaque appuis de touche sur le clavier
     * Effectue au total ng.longueur étapes
     *)
    let un_grain_clavier (p: param) (tas: t) (c: coord) (n: int): t =
        let source = tas |> dimensions |> (creer p) in
        deposer source 1 c;
        let attendre () =
            let _ = Graphics.wait_next_event[Key_pressed] in ()
        in
        animer tas n source attendre

    (* Dépose un à un les grain de sable dans tas dans la case c
     * attend dt secondes entre chaque étape
     * Effectue au total n étapes
     *)
    let un_grain_temps (p: param) (tas: t) (c: coord) (n: int) (dt: float): t =
        let source = tas |> dimensions |> (creer p) in
        deposer source 1 c;
        let attendre () =
            Unix.sleepf dt
        in
        animer tas n source attendre

    (* Calcule l'identite du groupe
     * des tas de sables recurrents de dimensions dim
     * en utilisant la formule : (2c_max - (2c_max)°)°
     *)
    let identite (p: param) (dim: coord): t =
        let double_max = creer p dim in
        iterer
            (fun c -> deposer double_max (2 * max_valeur double_max c) c)
            double_max;

        let stat_db_max = copier double_max in
        avalanche stat_db_max;

        (* Multiplie par -1 *)
        iterer
            (fun c -> deposer stat_db_max (-2 * (valeur stat_db_max c)) c)
            stat_db_max;

        sommer double_max stat_db_max

    (* Calcule le cardinal des tas de sable stables *)
    let cardinal_stables (p: param) (dim: coord): int =
        let graphe = G.creer p dim in
        reduire graphe (fun acc c -> acc * (max_valeur graphe c + 1)) 1

    (* Calcule le laplacien réduit de la grille *)
    let laplacien_reduit (p: param) (dim: coord): RationnalMatrix.matrix =
        (* L = out(G) - A^t *)
        let graphe = G.creer p dim in
        let n = graphe |> G.nb_cases in
        let l = RationnalMatrix.empty n n in

        iterer
            ( fun c ->
            let voisins_c = voisins graphe c in
            let lin_c = lineariser graphe c in

            (* out(G) *)
            let max_c = c |> (max_valeur graphe) |> ((+) 1) |> Q.of_int in
            RationnalMatrix.set_elt l (lin_c, lin_c) max_c;

            (* - A^t *)
            List.iter
                ( fun c_v ->
                let lin_c_v = lineariser graphe c_v in
                let coeff = (lin_c_v, lin_c) in
                RationnalMatrix.set_elt
                    l
                    coeff
                    (Q.sub (RationnalMatrix.get_elt l coeff) (Q.of_int 1))q
                )
                voisins_c
            )
            graphe;

        l

    (* Calcule le cardinal des tas de sable récurrents *)
    let cardinal_recurrents (p: param) (dim: coord): Z.t =
        laplacien_reduit p dim
        |> RationnalMatrix.determinant
        |> Q.to_bigint
        |> Z.abs

    (* Calcule le cardinal des tas de sable récurrents
     * et le rapport entre ceux récurrents et ceux stables
     *)
    let rapport_recurrents_stables (p: param) (dim: coord): Z.t * Q.t =
        let card_rec = cardinal_recurrents p dim in
        let graphe = creer p dim in
        let rapport_rec_stab = reduire graphe
            (fun acc c -> Q.div acc (Q.of_int (max_valeur graphe c + 1)))
            (Q.of_bigint card_rec)
            in
        card_rec, rapport_rec_stab

end
