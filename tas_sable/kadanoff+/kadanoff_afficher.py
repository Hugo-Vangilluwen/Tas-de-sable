import matplotlib
matplotlib.use("QtAgg")

import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle
import glob
import re
import os
#os.chdir("kadanoff+")

def afficher_tas_depuis_fichier(nom_fichier):
    with open(nom_fichier, "r") as f:
        n = int(f.readline())
        valeurs = list(map(int, f.readline().split()))

    fig, ax = plt.subplots(figsize=(12,6))

    for x in range(n):
        for y in range(valeurs[x]):
            ax.add_patch(
                Rectangle(
                    (x, y),
                    1,
                    1,
                    facecolor='gray',
                    edgecolor='black',
                    linewidth=0.5
                )
            )

    ax.set_xlim(0, n)
    ax.set_ylim(0, max(valeurs))

    ax.set_xticks([])
    ax.set_yticks([])
    ax.set_frame_on(False)

    ax.set_aspect('equal')

    nom_image = "images/" + nom_fichier.replace(".txt", ".png")

    fig.savefig(nom_image,
                dpi=300,
                bbox_inches='tight')

    plt.show()


def numero(f):
    return int(re.findall(r'\d+', f)[0])


def afficher_tas(valeurs, hauteur_max):
    plt.clf()
    ax = plt.gca()

    n = len(valeurs)

    for x in range(n):
        for y in range(valeurs[x]):
            ax.add_patch(
                Rectangle(
                    (x, y),
                    1, 1,
                    facecolor="gray",
                    edgecolor="black",
                    linewidth=0.5
                )
            )

    ax.set_xlim(0, n)
    ax.set_ylim(0, hauteur_max)

    ax.set_aspect("equal")
    ax.set_xticks([])
    ax.set_yticks([])
    ax.set_frame_on(False)

    plt.draw()


def lire_fichier(nom_fichier):
    with open(nom_fichier, "r") as f:
        n = int(f.readline())
        valeurs = list(map(int, f.readline().split()))
    return valeurs[:n]


def animation():
    files = sorted(glob.glob("stab_*.txt"), key=numero)

    if not files:
        print("Aucune frame trouvée.")
        return

    # dossier de sortie pour les images
    os.makedirs("images", exist_ok=True)

    # calcul du max global
    max_global = 0
    frames = []

    for f in files:
        v = lire_fichier(f)
        frames.append(v)
        max_global = max(max_global, max(v))

    plt.figure()

    for i, v in enumerate(frames):

        afficher_tas(v, max_global)

        # sauvegarde de la frame
        nom_image = f"images/frame_{i:03d}.png"
        plt.savefig(nom_image, dpi=200, bbox_inches='tight')

        plt.pause(1)

    plt.show()

animation()

#afficher_tas_depuis_fichier("intro_kadanoff.txt")
#afficher_tas_depuis_fichier("tas_ajoutid.txt")
#afficher_tas_depuis_fichier("deuxcmax.txt")
#afficher_tas_depuis_fichier("deuxcmax_s.txt")
#afficher_tas_depuis_fichier("id.txt")
#afficher_tas_depuis_fichier("deux_id.txt")
