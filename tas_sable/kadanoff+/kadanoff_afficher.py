import matplotlib
matplotlib.use("QtAgg")

import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle
import glob
import re

#import os
#os.chdir("kadanoff+")
#print(os.getcwd())

def afficher_tas_depuis_fichier(nom_fichier):
    with open(nom_fichier, "r") as f:
        n = int(f.readline())
        valeurs = list(map(int, f.readline().split()))

    fig, ax = plt.subplots(figsize=(12,6))

    # Dessin des carrés
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

    # Limites propres (grille discrète)
    ax.set_xlim(0, n)
    ax.set_ylim(0, max(valeurs))

    # Suppression des axes “scientifiques”
    ax.set_xticks([])
    ax.set_yticks([])
    ax.set_frame_on(False)

    ax.set_aspect('equal')

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
    files = sorted(glob.glob("frame_*.txt"), key=numero)

    if not files:
        print("Aucune frame trouvée.")
        return

    # 🔥 calcul du max global AVANT animation
    max_global = 0
    frames = []

    for f in files:
        v = lire_fichier(f)
        frames.append(v)
        max_global = max(max_global, max(v))

    plt.figure()

    for v in frames:
        afficher_tas(v, max_global)
        plt.pause(1)

    plt.show()

#animation()

#afficher_tas_depuis_fichier("id25.txt")
#afficher_tas_depuis_fichier("tas_ajoutid.txt")
#afficher_tas_depuis_fichier("deuxcmax.txt")
#afficher_tas_depuis_fichier("deuxcmax_s.txt")
afficher_tas_depuis_fichier("id.txt")
afficher_tas_depuis_fichier("deux_id.txt")