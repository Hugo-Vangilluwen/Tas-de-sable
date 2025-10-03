#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jun 24 17:57:02 2024

@author: hugo
"""

import sand_pile as sp


def sources_identite(n):
    """Calcule les sources permettant d'obtenir l'identité de taille n"""
    s = sp.SandPile(n)

    for i in range(n):
        s.add_source((i, 0))
        s.add_source((i, n-1))
        s.add_source((0, i))
        s.add_source((n-1, i))

    return s

def identite(n):
    """Calcule l'identité de taille n"""
    s = sources_identite(n)

    while True:
        tmp = s.copy()
        s.activate()
        if (tmp == s).all():
            break

    return s
