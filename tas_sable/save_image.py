#!/usr/bin/env python3

import sys
import numpy as np
from PIL import Image


with open("tmp_list_img") as f:
    array = np.array(eval(f.read()), dtype=np.uint8)
    img = Image.fromarray(array)
    img.save("./images/" + sys.argv[1])


