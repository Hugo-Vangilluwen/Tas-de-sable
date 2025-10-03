#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author: hugo
"""


import numpy as np
import matplotlib.pyplot as plt
from matplotlib import animation


class Lake:
    """Modélisation des lacs abéliens"""

    def __init__(self, _size, _flow_func, _influence_size, _time_step, _source=False):
        """Initialise un lac avec
        - _size : un nombre de surfaces mésoscopiques
        - _flow_func : un fonction du taux de croissance de l'écoulement de [0;8] dans [0;1]
        - _time_step : un pas de temps
        - _filter : un filtre sous forme de matrice de _size * _size"""
        self.size = _size
        self.grid = np.zeros((self.size, self.size), float)
        if _source:
            self.sources = None
        else:
            self.sources = Lake(self.size, None, None, None, True)

        self.flow_func = _flow_func
        self.time_step = _time_step

        self.influence = _influence_size

    def is_source(self):
        """Teste si ce tas de sable est une source"""
        return self.sources is None

    def _flow_centered(self, g):
        """Calcule l'écoulement centré en g."""
        result = np.zeros((self.size, self.size), float)

        for i in range(-self.influence, self.influence+1):
            for j in range(-self.influence, self.influence+1):
                x = i + g[0]
                y = j + g[1]
                if 0 <= x < self.size and 0 <= y < self.size:
                    result[x][y] = self.flow_func((i, j), self.grid[g[0]][g[1]])

        return result

    def flow(self):
        """"Calcule un écoluement"""
        sum_flow = np.zeros((self.size, self.size), float)

        for i in range(self.size):
            for j in range(self.size):
                sum_flow += self._flow_centered((i, j))

        self.grid += sum_flow * self.time_step

    def __add__(self, other):
        """Somme de deux lacs.
        Cette opération n'est pas commutative."""
        assert self.size == other.size

        result = Lake(self.size, self.flow_func, self.time_step, self.influence)

        for i in range(self.size):
            for j in range(self.size):
                result.grid[i][j] = self.grid[i][j] + other.grid[i][j]

        return result

    def __iadd__(self, other):
        """Ajoute un autre lacs à celui-ci"""
        self.grid = (self + other).grid.copy()

        return self

    def __eq__(self, other):
        """Teste si self et other sont égaux"""
        return self.grid == other.grid

    def copy(self):
        """Copie profonde"""
        result = Lake(self.size, self.flow_func, self.influence, self.time_step, self.is_source())

        result.grid = self.grid.copy()
        if not self.is_source():
            result.sources = self.sources.copy()

        return result

    def drop(self, g, value):
        """Ajoute la goutte d'eau g"""
        self.grid[g[0]][g[1]] += value

    def add_source(self, s, value):
        """Ajoute une goutte d'eau s aux sources"""
        assert not self.is_source()
        self.sources.drop(s, value)

    def set_source(self, s):
        """Fixe s comme source"""
        assert self.size == s.size
        self.sources = s

    def activate(self):
        """Active une fois les sources et calcule les écoulements"""
        self += self.sources
        self.flow()

    def generate(self):
        """Génère une source"""
        # utile pour d'autre type de sources
        assert self.is_source()

    def show(self, axes=plt, vmin=-1, vmax=1, animated=False):
        """Calcule l'image représentant ce tas de sable"""
        return axes.imshow(self.grid, animated=animated, interpolation="nearest",
                          norm="linear", vmin=vmin, vmax=vmax, cmap="winter_r")

    def animate(self, times, speed, vmin=-1, vmax=1, video_name=None):
        """Affiche une animation de l'écoulement du lac
        Et l'enregistre si une chaîne de caractères est donnée en argument"""
        fig = plt.figure()
        images = [ [self.show(animated=True)] ]

        for _ in range(times):
            self.sources.generate()
            self.activate()
            images.append([self.show(vmin=vmin, vmax=vmax, animated=True)])

        ani = animation.ArtistAnimation(fig, images, interval=1000/speed, blit=True,
                                        repeat_delay=1000)

        if isinstance(video_name, str) and video_name != "":
            ani.save(video_name + ".mp4")

        plt.show()

    def __repr__(self):
        """Représente ce tas de sable"""
        return repr(self.grid)
