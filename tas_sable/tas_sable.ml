(* Type representant les coordonnees d'une case *)
type coord = int * int

(* Signature d'une structure de grille *)
module type GRILLE = sig
    (* Type representant une grille *)
    type t

    (* Nombre maximum de voisin moins un
     * c'est-à-dire la valeur maximal de la case
     *)
    val max_valeur : t -> coord -> int

    (* Cree une grille de dimension n x m *)
    val creer : coord -> t

    (* Renvoie la valeur de la case de coordonnees c *)
    val valeur : t -> coord -> int

    (* Modifie la valeur de la case de coordonnees c *)
    val deposer : t -> int -> coord -> unit

    (* Teste si la coordonnee est correcte *)
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

    (* Ouvre une fenetre Graphics de la bonne taille *)
    val ouvrir_fenetre : t -> unit

    (* Affiche la grille dans une fenetre graphique
     * Les valeurs de la grille doivent etre entre 0 et max_voisin - 1
     * Si une seconde grille est donnee, seuls les cases de valeurs differentes
     * sont redessinees
     *)
    val afficher_grille : t -> t option -> unit
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
    let (+) (tas1: t) (tas2: t) =
        let somme = superposer tas1 tas2 in
        avalanche somme;
        somme

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
            let tas_tmp = !tas_anime + source in
            afficher_grille tas_tmp (Some(!tas_anime));
            tas_anime := tas_tmp;
            attendre ()
        done;

        let _ = Graphics.wait_next_event[Key_pressed] in ();

        Graphics.close_graph ();
        !tas_anime

    (* Depose un à un les grain de sable dans tas dans la case c
     * change d'etape à chaque appuis de touche sur le clavier
     *)
    let un_grain_clavier (tas: t) (c: coord) (n: int): t =
        let source = tas |> dimensions |> creer in
        deposer source 1 c;
        let attendre () =
            let _ = Graphics.wait_next_event[Key_pressed] in ()
        in
        animer tas n source attendre

    (* Depose un à un les grain de sable dans tas dans la case c
     * attend dt secondes entre chaque etape
     *)
    let un_grain_temps (tas: t) (c: coord) (n: int) (dt: float): t =
        let source = tas |> dimensions |> creer in
        deposer source 1 c;
        let attendre () =
            Unix.sleepf dt
        in
        animer tas n source attendre

    (* Calcule l'identite du groupe
     * des tas de sables recurrents de dimensions dim
     * en utilisant la formule : (2c_max - (2c_max)°)°
     *)
    let identite (dim: coord): t =
        let double_max = creer dim in
        iterer
            (fun c -> deposer double_max (2 * max_valeur double_max c) c)
            double_max;

        let stat_db_max = copier double_max in
        avalanche stat_db_max;

        (* Multiplie par -1 *)
        iterer
            (fun c -> deposer stat_db_max (-2 * (valeur stat_db_max c)) c)
            stat_db_max;

        double_max + stat_db_max


end
