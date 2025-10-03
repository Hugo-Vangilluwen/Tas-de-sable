#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author: hugo
"""

import sand_pile as sp
import random_sand
import identite
import matplotlib.pyplot as plt
import time



if __name__ == "__main__":
    # identite.identite(31).show()
    # plt.show()

    # r = random_sand.RandomSand(21, 3)
    # t = sp.SandPile(21)
    # t.set_source(r)
    # t.animate(1000, 50)

    t = sp.SandPile(51)
    t.add_source((25,25))
    t.animate(500, 20)

    # # Maxime size window
    # # for 'TkAgg' backend
    # plt.figure(1)
    # plt.switch_backend('TkAgg') #TkAgg (instead Qt4Agg)
    # mng = plt.get_current_fig_manager()
    # mng.resize(*mng.window.maxsize())
    #
    # fig, axarr = plt.subplots(3, 4, num=1)
    # for i in range(12):
    #     identite.identite((i+1) * 5).show(axes=axarr[i//4, i%4])
    #     axarr[i//4, i%4].set_title(f'n={(i+1)*5}')
    #
    #
    # plt.show()
