import numpy as np
import matplotlib.pyplot as plt
from datetime import datetime
import sys

# datetime.strptime

fh = open("hashperf.txt", "r")

x_key = []
y_occur = []

for line in fh:
    nline_str = line.replace("  ", " ").replace("  ", " ").replace("  ", " ").replace("  ", " ")
    nline = nline_str.split(" ")
    # ['At', 'key', '1', 'found', '1', 'allocated', 'k/v', 'pairs:\n']
    x_key.append(int(nline[2]))
    y_occur.append(int(nline[4]))

fig1 = plt.figure()
ax1 = fig1.add_subplot(111)
ax1.scatter(x_key, y_occur, label="Values within Hashed Key")

ax1.set_title('Hashing Performance')
ax1.legend()

plt.show()
