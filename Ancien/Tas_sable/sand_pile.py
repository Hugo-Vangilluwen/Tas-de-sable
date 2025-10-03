#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author: hugo
"""


import numpy as np
import matplotlib.pyplot as plt
from matplotlib import animation


class SandPile:
    """Modélisation des tas de sable abéliens"""

    def __init__(self, _size, _source=False):
        """Initialise un tas de sable avec sa taille"""
        self.size = _size
        self.grid = np.zeros((self.size, self.size), int)

        if _source:
            self.sources = None
        else:
            self.sources = SandPile(self.size, True)

    def is_source(self):
        """Teste si ce tas de sable est une source"""
        return self.sources is None

    def _landslide(self):
        """Effectue les écoulements et renvoie vrai si il y en a eu"""
        landslide = False

        for i in range(self.size):
            for j in range(self.size):
                if self.grid[i][j] >= 4:
                    self.grid[i][j] -= 4
                    landslide = True
                    if i > 0:
                        self.grid[i-1][j] += 1
                    if i < self.size - 1:
                        self.grid[i+1][j] += 1
                    if j > 0:
                        self.grid[i][j-1] += 1
                    if j < self.size - 1:
                        self.grid[i][j+1] += 1

        return landslide

    def __add__(self, other):
        """Somme de deux tas de sable"""
        assert self.size == other.size

        result = SandPile(self.size)

        for i in range(self.size):
            for j in range(self.size):
                result.grid[i][j] = self.grid[i][j] + other.grid[i][j]

        # Les écoulements sont toujours en nombre finis
        while result._landslide():
            pass

        return result

    def __iadd__(self, other):
        """Ajoute un autre tas de sable à celui-ci"""
        self.grid = (self + other).grid.copy()

        return self

    def __eq__(self, other):
        """Teste si self et other sont égaux"""
        return self.grid == other.grid

    def copy(self):
        """Copie profonde"""
        result = SandPile(self.size, self.is_source())

        result.grid = self.grid.copy()
        if not self.is_source():
            result.sources = self.sources.copy()

        return result

    def grain(self, g):
        """Ajoute le grain de sable g"""
        self.grid[g[0]][g[1]] += 1

    def add_source(self, s):
        """Ajoute un grain de sable s aux sources"""
        assert not self.is_source()
        self.sources.grain(s)

    def set_source(self, s):
        """Fixe s comme source"""
        assert self.size ==  s.size
        self.sources = s

    def activate(self):
        """Active une fois les sources"""
        self += self.sources

    def generate(self):
        """Génère une source"""
        assert self.is_source()

        pass # utile pour d'autre type de sources

    def show(self, axes=plt, animated=False):
        """Calcule l'image représentant ce tas de sable"""
        return axes.imshow(self.grid, animated=animated, interpolation="nearest",
                          norm="linear", vmin=0, vmax=3, cmap="gray_r")

    def animate(self, times, speed, video_name=None):
        """Affiche une animation de l'écoulement du tas de sable
        Et l'enregistre si une chaîne de caractères est donnée en argument"""
        fig = plt.figure()
        images = [ [self.show(animated=True)] ]

        for _ in range(times):
            self.sources.generate()
            self.activate()
            images.append([self.show(animated=True)])

        ani = animation.ArtistAnimation(fig, images, interval=1000/speed, blit=True,
                                        repeat_delay=1000)

        if isinstance(video_name, str) and video_name != "":
            ani.save(video_name + ".mp4")

        plt.show()

    def __repr__(self):
        """Représente ce tas de sable"""
        return repr(self.grid)
