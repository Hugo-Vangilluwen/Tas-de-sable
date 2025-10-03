#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author: hugo
"""

import sand_pile as sp
import random
import numpy as np


class RandomSand(sp.SandPile):
    """Modélisation d'un tas de sable aléatoire
    Très utile comme source"""

    def __init__(self, _size, _pinch, _law=random.randint):
        """Initialise un tas de sable avec sa taille et la taille de la pincée aléatoire"""
        self.size = _size
        self.sources = None
        self.pinch = _pinch # pincée
        self.law = _law

        self.generate()

    def grain(self, other_sand_pile):
        """Surcharge la méthode de la classe parente"""
        raise AttributeError("Impossible d'ajouter un grain à un tas de sable aléatoire")

    def generate(self):
        """Génère une pincée aléatoire"""
        self.grid = np.zeros((self.size, self.size), int)

        for _ in range(self.pinch):
            self.grid [self.law(0, self.size - 1)] [self.law(0, self.size - 1)] += 1

