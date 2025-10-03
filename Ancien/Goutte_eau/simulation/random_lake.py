#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author: hugo
"""


import random
import numpy as np
import lake


class RandomLake(lake.Lake):
    """Modélisation d'un lac aléatoire
    Très utile comme source"""

    def __init__(self, _size, _pinch, _law=random.randint):
        """Initialise un lac avec sa taille et la taille de la pincée aléatoire"""
        super().__init__(_size, None, None, None, True)

        self.pinch = _pinch # pincée
        self.law = _law

        self.generate()

    def drop(self, g, value):
        """Surcharge la méthode de la classe parente"""
        raise AttributeError("Impoosible d'ajouter un grain à un tas de sable aléatoire")

    def generate(self):
        """Génère une pincée aléatoire"""
        self.grid = np.zeros((self.size, self.size), int)

        for _ in range(self.pinch):
            self.grid [self.law(0, self.size - 1)] [self.law(0, self.size - 1)] += 1

