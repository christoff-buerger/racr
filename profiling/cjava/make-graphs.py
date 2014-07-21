#!/usr/bin/python2.7 -tt
# -*- coding: utf-8 -*-

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

import sys
import getopt
import collections
import numpy as np
import matplotlib.pyplot as plt

# Parse command line arguments:
x = y = l = ''
try:
    opts, args = getopt.getopt(sys.argv[1:], 'x:y:l:')
    for opt, arg in opts:
        if opt == '-x':
            x = arg
        elif opt == '-y':
            y = arg
        elif opt == '-l':
            l = arg
    if x == '' or y == '' or l == '':
        raise getopt.GetoptError('', None)
except getopt.GetoptError:
    print 'Usage: -x x-axis (mandatory)'
    print '       -y y-axis (mandatory)'
    print '       -l Line labels (mandatory)'
    sys.exit(2)

# Read combined measurements:
data = np.genfromtxt('measurements/measurements.table',
                     delimiter='|',
                     names=True,
                     comments='--',
                     dtype=None,
                     autostrip=True)

# Plot graph:
markers = collections.deque(['D', 's', '^', 'd', 'o', '*'])
for l_name in set(data[l]):
    l_points = sorted(filter(lambda row: row[l] == l_name, data),
                      lambda row1, row2: 1 if row1[x] > row2[x] else -1)
    plt.plot(map(lambda row: row[x], l_points),
             map(lambda row: row[y], l_points),
             '-' + markers[0],
             label=l_name)
    markers.rotate(-1)
    for row in l_points:
        plt.axhspan(row[y], row[y], color='lightgrey', linestyle='dotted')
plt.xlabel(x)
plt.ylabel(y)
plt.legend(loc='upper left')
#plt.gca().grid(True)
plt.savefig('measurements/measurements.pdf',
            bbox_inches='tight',
            transparent=True,
            format='pdf')
#plt.show()
