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
from mpl_toolkits.mplot3d import axes3d
from matplotlib import cm

# Parse command line arguments:
x = y = z = l = ''
try:
    opts, args = getopt.getopt(sys.argv[1:], 'x:y:z:l:')
    for opt, arg in opts:
        if opt == '-x':
            x = arg
        elif opt == '-y':
            y = arg
        elif opt == '-z':
            z = arg
        elif opt == '-l':
            l = arg
    if x == '' or y == '' or l == '':
        raise getopt.GetoptError('', None)
except getopt.GetoptError:
    print 'Usage: -x x-axis (mandatory)'
    print '       -y y-axis (mandatory)'
    print '       -z z-axis (optional)'
    print '       -l Line labels (mandatory)'
    sys.exit(2)

# Read combined measurements:
data = np.genfromtxt('measurements/combined-measurements.table',
                     delimiter='|',
                     names=True,
                     comments='--',
                     dtype=None,
                     autostrip=True)

markers = collections.deque(['D', 's', '^', 'd', 'o', '*'])
colors = collections.deque(['b', 'g', 'r', 'c'])

def plot2d(data):
    plt.gca()
    for l_value in set(data[l]):
        l_points = sorted(filter(lambda row: row[l] == l_value, data),
                          lambda row1, row2: 1 if row1[x] > row2[x] else -1)
        plt.plot(map(lambda row: row[x], l_points),
                 map(lambda row: row[y], l_points),
                 '-' + markers[0],
                 color=colors[0],
                 label=l_value)
        for row in l_points:
            plt.axhspan(row[y], row[y], color='lightgrey', linestyle='dotted')
        markers.rotate(-1)
        colors.rotate(-1)
    plt.xlabel(x)
    plt.ylabel(y)
    plt.legend()

def plot3d(data):
    fig = plt.figure()
    ax = fig.gca(projection='3d')
    lines = []
    labels = []
    for l_value in set(data[l]):
        l_points = sorted(filter(lambda row: row[l] == l_value, data),
                          lambda row1, row2: 1 if row1[x] > row2[x] else -1)
        x_values = []
        y_values = []
        z_values = []
        for y_value in sorted(set(map(lambda row: row[y], l_points)), lambda y1, y2: 1 if y1 > y2 else -1):
            ly_points = filter(lambda row: row[y] == y_value, l_points)
            x_line = map(lambda row: row[x], ly_points)
            y_line = [y_value] * len(ly_points)
            z_line = map(lambda row: row[z], ly_points)
            x_values.append(x_line)
            y_values.append(y_line)
            z_values.append(z_line)
            #line = ax.plot(x_line, y_line, z_line, '-', alpha=0.5, color=colors[0])
        #fewest_measurements = len(min(x_values, key=lambda a: len(a)))
        #x_values = map(lambda v:v[:fewest_measurements], x_values)
        #y_values = map(lambda v:v[:fewest_measurements], y_values)
        #z_values = map(lambda v:v[:fewest_measurements], z_values)
        ax.plot_surface(x_values, y_values, z_values, alpha=0.15, color=colors[0])
        ax.plot_wireframe(x_values, y_values, z_values, alpha=0.7, color=colors[0])
        lines.append(plt.Line2D((0,1),(0,0), color=colors[0], linewidth=4.0))
        labels.append(l_value)
        markers.rotate(-1)
        colors.rotate(-1)
    ax.set_xlabel(x)
    ax.set_ylabel(y)
    ax.set_zlabel(z)
    ax.legend(lines, labels, loc='upper left')

# Plot graph:
if z == '':
    plot2d(data)
else:
    plot3d(data)
plt.savefig('measurements/combined-measurements.pdf',
            bbox_inches='tight',
            transparent=True,
            format='pdf')
plt.show()
