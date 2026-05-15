#!/usr/bin/env python3

import sys
import os
import numpy as np
from PIL import Image


img_path = "./images/" + sys.argv[1]
assert(not os.path.exists(img_path))

with open("tmp_list_img") as f:
    res = []
    for line in f.read().split("|"):
        if not line:
            continue

        line_data = []
        res.append(line_data)

        for cell in line.split(";"):
            if not cell:
                continue

            line_data.append(tuple(cell.split(",")))


    array = np.array(res, dtype=np.uint8)
    img = Image.fromarray(array)
    img.save(img_path)

