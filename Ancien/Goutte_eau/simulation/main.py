#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author: hugo
"""


import time
import matplotlib.pyplot as plt
import lake
import random_lake


def carac(i, hauteur):
    if i == (0,0):
        return -hauteur
    if i[0] == 0 or i[1] == 0:
        return hauteur / 4
    return 0


if __name__ == "__main__":
    t = lake.Lake(21, carac, 1, 1)
    t.add_source((10, 10), 1)
    t.animate(100, 1, 0, 1)

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
