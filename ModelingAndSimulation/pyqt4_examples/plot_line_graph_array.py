# -*- coding: UTF-8 -*-

"""

Plotting more than one plot on the same set of axes
http://www.ast.uct.ac.za/~sarblyth/pythonGuide/PythonPlottingBeginnersGuide.pdf

"""

import pylab

# Make x, y arrays for each graph
x1 = [1, 2, 3, 4, 5]
y1 = [1, 4, 9, 16, 25]
x2 = [1, 2, 4, 6, 8]
y2 = [2, 4, 8, 12, 16]

# use pylab to plot x and y
pylab.plot(x1, y1, 'r', linewidth=1.0)
pylab.plot(x2, y2, 'g', linewidth=1.0)

# give plot a title
pylab.title('Plot of y vs. x')

# make axis labels
pylab.xlabel('x axis')
pylab.ylabel('y axis')

# set axis limits
pylab.xlim(0.0, 9.0)
pylab.ylim(0.0, 30.)

# show the plot on the screen
pylab.show()

