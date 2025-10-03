#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Fichier testant les différentes fonctions et classes du projet
@author: hugo
"""


import matplotlib.pyplot as plt

import identite


def test_identite():
    """Teste du module identite"""
    identite.identite(8).show()
    plt.show()
    identite.identite(9).show()
    plt.show()
    identite.identite(26).show()
    plt.show()


if __name__ == "__main__":
    test_identite()
