#! /usr/bin/env python
# -*- coding: utf-8 -*-
#

#
# Licensing
#
#   Copyright 2017 @ Evandro Coan
#
#  This program is free software; you can redistribute it and/or modify it
#  under the terms of the GNU General Public License as published by the
#  Free Software Foundation; either version 3 of the License, or ( at
#  your option ) any later version.
#
#  This program is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
#  General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

import math
import pylab
import numpy
import random

from settings      import *
from matplotlib    import pyplot
from progress_bar  import ProgressBar
from drawing_panel import DrawingPanel

from PyQt4 import QtGui, QtCore, Qt
log( 1, "Importing " + __name__ )


class Simulator():

    def __init__( self, mainWindow, drawingPanel ):
        """
            Creates the simulations and draw the path on the drawing panel.

            @param mainWindow      is to get the data form the fields
            @param drawingPanel    is the panel where the drawing will be put

            @throws error when some input data is invalid
        """
        self.maxAngle = 2 * math.pi
        self.allIterationsResult = []
        self.firstIterationSteps = []

        self.mainWindow = mainWindow
        self.drawingPanel = drawingPanel

        log( 2, "( Simulator::__init__ ) self.maxAngle: " + repr( self.maxAngle ) )

    def startSimulation( self ):
        """
            Begin the simulation process.
        """
        howManySteps = int( self.mainWindow.stepNumberLineEdit.text() )
        howManyTimes = int( self.mainWindow.replicationsNumberLineEdit.text() )

        progressBar = ProgressBar( None, howManyTimes, howManySteps )
        progressBar.show()

        # Set it 100% it will not be updated for performance increase
        if howManySteps <= MINIMUM_STEPS_TO_SHOW_PARTIAL_PROGRESS:
            progressBar.progressBarPartial.setValue( howManySteps )

        self.mainWindow.handleClearView( True )

        # Why is local variable access faster than class member access in Python?
        # https://stackoverflow.com/questions/12397984/why-is-local-variable-access-faster-than-class-member-access-in-python
        #
        # My Python for loop is causing a MemoryError. How can I optimize this?
        # https://stackoverflow.com/questions/4405083/my-python-for-loop-is-causing-a-memoryerror-how-can-i-optimize-this
        for iterationCount in xrange( 0, howManyTimes ):

            # Stops the process when the cancel button is hit
            if self.runOneIteration( iterationCount, howManySteps, howManyTimes, progressBar ) \
                    or progressBar.incrementBarOverall():
                break

        itemsBounding = self.drawingPanel.fitAxes()

        self.showResults( itemsBounding, howManySteps )
        self.mainWindow.fitSceneInView( itemsBounding )

        if howManyTimes > 1:
            self.plotHistogram( howManyTimes )

        else:
            self.plotWalkedPath( howManySteps )

    def showResults( self, itemsBounding, howManySteps ):
        """
            addText() change the text color inside a QGraphicsView
            https://stackoverflow.com/questions/27612052/addtext-change-the-text-color-inside-a-qgraphicsview
        """
        results = QtGui.QGraphicsTextItem()
        topLeft = itemsBounding.topLeft()
        bottomRight = itemsBounding.bottomRight()

        x = increaseAxe( topLeft.x(), bottomRight.x(), 160 )
        y = increaseAxe( topLeft.y(), bottomRight.y(), 80 )

        results.setPos( x, y )
        results.setPlainText( "A distancia final percorrida: %f\n\nA diferenca para a distancia estimada: %f" %
                ( self.firstIterationSteps[-1], abs( self.firstIterationSteps[-1] - math.sqrt( howManySteps ) ) ) )

        self.drawingPanel.scene.addItem( results )

    def plotWalkedPath( self, howManySteps ):
        """
            Plotting more than one plot on the same set of axes
            http://www.ast.uct.ac.za/~sarblyth/pythonGuide/PythonPlottingBeginnersGuide.pdf

            Matplotlib: linewidth is added to the length of a line
            https://stackoverflow.com/questions/10297220/matplotlib-linewidth-is-added-to-the-length-of-a-line
        """

        # Make x, y arrays for each graph
        x1 = range( 0, howManySteps )
        y1 = self.firstIterationSteps

        # use pylab to plot x and y
        pylab.plot(x1, y1, 'g', linewidth=0.5)

        # give plot a title
        pylab.title('Plot of y vs. x')

        # make axis labels
        pylab.xlabel('x axis')
        pylab.ylabel('y axis')

        # show the plot on the screen
        pylab.show()

    def plotHistogram( self, howManyTimes ):
        """
            https://matplotlib.org/api/pyplot_api.html

            How does numpy.histogram() work?
            https://stackoverflow.com/questions/9141732/how-does-numpy-histogram-work

            Python histogram outline
            https://stackoverflow.com/questions/42741687/python-histogram-outline
        """
        histogramClasses = int( math.ceil( math.sqrt( howManyTimes ) ) )

        # Set the maximum classes limit to 30
        if histogramClasses > 30:
            histogramClasses = 30

        # log( 2, "( Simulator::plotHistogram ) histogramClasses:         " + str( histogramClasses ) )
        # log( 2, "( Simulator::plotHistogram ) self.allIterationsResult: " + str( self.allIterationsResult ) )

        # hist, bin_edges = numpy.histogram( self.allIterationsResult, bins=histogramClasses )
        # log( 2, "( Simulator::plotHistogram ) hist:      " + str( hist ) )
        # log( 2, "( Simulator::plotHistogram ) bin_edges: " + str( bin_edges ) )
        # pyplot.bar( bin_edges[:-1], hist )

        pyplot.hist( self.allIterationsResult, bins=histogramClasses, edgecolor='black', linewidth=1.2 )
        pyplot.show()

    def runOneIteration( self, iterationCount, howManySteps, howManyTimes, progressBar ):
        x = 0.0
        y = 0.0
        pathLength = 0.0

        progress_bar_trigger = MINIMUM_STEPS_TO_SHOW_PARTIAL_PROGRESS / 1000

        def computeLineWithHistogram():
            pathLength = self.getPointsDistance( 0, x, 0, y )
            self.firstIterationSteps.append( pathLength )

        if iterationCount > 0 or howManyTimes > 1:

            if howManySteps > MINIMUM_STEPS_TO_SHOW_PARTIAL_PROGRESS:

                def addLine():
                    return progressBar.incrementBarParcial()

            else:

                def addLine():
                    pass

        else:

            if howManySteps > MINIMUM_STEPS_TO_SHOW_PARTIAL_PROGRESS:

                def addLine():
                    computeLineWithHistogram()
                    self.drawingPanel.drawLine( x_old, y_old, x, y )

                    return progressBar.incrementBarParcial()

            else:

                def addLine():
                    computeLineWithHistogram()
                    self.drawingPanel.drawLine( x_old, y_old, x, y )

        for index in xrange( 0, howManySteps ):
            randomAngle = self.getRandomAngle()
            randomDegree = math.degrees( randomAngle )
            # log( 2, "( Simulator::runOneIteration ) randomAngle: %20s (%14f°)" % ( repr( randomAngle ), randomDegree ) )

            x_old = x
            y_old = y
            x += math.cos( randomAngle )
            y += math.sin( randomAngle )

            if addLine():
                return True

        pathLength = self.getPointsDistance( 0, x, 0, y )
        self.allIterationsResult.append( pathLength )

        # log( 2, "( Simulator::runOneIteration ) x: %14f, y: %14f, pathLength: %14f" % ( x, y, pathLength ) )
        return False

    def getRandomAngle( self ):
        return random.uniform(0, self.maxAngle)

    def getPointsDistance( self, x1, y1, x2, y2 ):
        return math.hypot( x2 - x1, y2 - y1 )



