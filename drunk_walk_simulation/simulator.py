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
import matplotlib

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
        self.isSimulationRunning = False

        log( 2, "( Simulator::__init__ ) self.maxAngle: " + repr( self.maxAngle ) )

    def startSimulation( self ):
        """
            Begin the simulation process.
        """
        self.isSimulationRunning = True
        matplotlib.pyplot.close("all")

        self.startSimulation_()
        self.isSimulationRunning = False

    def startSimulation_( self ):

        howManySteps = int( self.mainWindow.stepNumberLineEdit.text() )
        howManyTimes = int( self.mainWindow.replicationsNumberLineEdit.text() )

        lastInterations     = howManySteps % MINIMUM_STEPS_TO_SHOW_PARTIAL_PROGRESS
        totalFullIterations = int( howManySteps / MINIMUM_STEPS_TO_SHOW_PARTIAL_PROGRESS )
        totalIterations     = totalFullIterations + ( 1 if lastInterations > 0 else 0 )

        progressBar = ProgressBar( None, howManyTimes, totalIterations )
        progressBar.show()

        # Set it 100% it will not be updated for performance increase
        if howManySteps <= MINIMUM_STEPS_TO_SHOW_PARTIAL_PROGRESS:
            progressBar.progressBarPartial.setValue( totalIterations )

        isToDrawLines = howManyTimes < 2

        if isToDrawLines:
            self.mainWindow.handleClearView( True )

        else:
            self.mainWindow.handleClearView()


        # Why is local variable access faster than class member access in Python?
        # https://stackoverflow.com/questions/12397984/why-is-local-variable-access-faster-than-class-member-access-in-python
        #
        # My Python for loop is causing a MemoryError. How can I optimize this?
        # https://stackoverflow.com/questions/4405083/my-python-for-loop-is-causing-a-memoryerror-how-can-i-optimize-this
        for iterationCount in xrange( 0, howManyTimes ):

            # Stops the process when the cancel button is hit
            if self.runOneIteration( isToDrawLines, totalFullIterations, lastInterations, progressBar ) \
                    or progressBar.incrementBarOverall():
                break

        if isToDrawLines:
            itemsBounding = self.drawingPanel.fitAxes()
            self.mainWindow.fitSceneInView( itemsBounding )

            self.showResults( itemsBounding, howManySteps )
            self.plotWalkedPath( howManySteps )

        else:
            self.plotHistogram( howManyTimes )

    def showResults( self, itemsBounding, howManySteps ):
        """
            addText() change the text color inside a QGraphicsView
            https://stackoverflow.com/questions/27612052/addtext-change-the-text-color-inside-a-qgraphicsview
        """
        results = QtGui.QGraphicsTextItem()
        topLeft = itemsBounding.topLeft()

        bottomRight     = itemsBounding.bottomRight()
        estimatedLength = math.sqrt( howManySteps )

        x = increaseAxe( topLeft.x(), bottomRight.x(), 160 )
        y = increaseAxe( topLeft.y(), bottomRight.y(), 80 )

        results.setPos( x, y )
        results.setPlainText( "A distancia final percorrida: %f\n\nA diferenca para a distancia estimada (%f) eh de: %f" %
                ( self.firstIterationSteps[-1], estimatedLength, self.firstIterationSteps[-1] - estimatedLength ) )

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
        y2 = []

        for x in x1:
            y2.append( math.sqrt( x ) )

        # use pylab to plot x and y
        pylab.plot(x1, y1, 'g', linewidth=0.5, label="Drunk Path")
        pylab.plot(x1, y2, 'b', linewidth=0.5, label="Raiz de N")

        # give plot a title
        pylab.title('Plot of y vs. x')

        # Adding a legend to PyPlot in Matplotlib in the most simple manner possible
        # https://stackoverflow.com/questions/19125722/adding-a-legend-to-pyplot-in-matplotlib-in-the-most-simple-manner-possible
        pylab.legend(loc='upper left')

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

        if len( self.allIterationsResult ) < 1:
            log( 2, "( Simulator::plotHistogram ) not enough data: " + str( self.allIterationsResult ) )
            return

        # log( 2, "( Simulator::plotHistogram ) histogramClasses:         " + str( histogramClasses ) )
        # log( 2, "( Simulator::plotHistogram ) self.allIterationsResult: " + str( self.allIterationsResult ) )

        # hist, bin_edges = numpy.histogram( self.allIterationsResult, bins=histogramClasses )
        # log( 2, "( Simulator::plotHistogram ) hist:      " + str( hist ) )
        # log( 2, "( Simulator::plotHistogram ) bin_edges: " + str( bin_edges ) )
        # pyplot.bar( bin_edges[:-1], hist )

        pyplot.hist( self.allIterationsResult, bins=histogramClasses, edgecolor='black', linewidth=1.2 )
        pyplot.show()

    def runOneIteration( self, isToDrawLines, totalFullIterations, lastInterations, progressBar ):
        """
            Initializing a list to a known number of elements in Python [duplicate]
            https://stackoverflow.com/questions/521674/initializing-a-list-to-a-known-number-of-elements-in-python
        """
        x     = [0.0]
        y     = [0.0]
        x_old = [0.0]
        y_old = [0.0]

        pathLength            = 0.0
        isToUpdateProgressBar = totalFullIterations > 1

        # log( 2, "( Simulator::runOneIteration ) isToDrawLines:       %d" % ( isToDrawLines ) )
        # log( 2, "( Simulator::runOneIteration ) lastInterations:     %d" % ( lastInterations ) )
        # log( 2, "( Simulator::runOneIteration ) totalFullIterations: %d" % ( totalFullIterations ) )

        def computeLineWithHistogram():
            pathLength = self.getPointsDistance( 0, x[0], 0, y[0] )
            self.firstIterationSteps.append( pathLength )

        if isToDrawLines:

            def addLine():
                computeLineWithHistogram()
                self.drawingPanel.drawLine( x_old[0], y_old[0], x[0], y[0] )

        else:

            def addLine():
                pass

        # scoping error in recursive closure
        # https://stackoverflow.com/questions/2516652/scoping-error-in-recursive-closure
        def iterationFullStep( howManySteps ):

            for index in xrange( 0, howManySteps ):
                randomAngle = self.getRandomAngle()
                randomDegree = math.degrees( randomAngle )
                # log( 2, "( Simulator::runOneIteration ) randomAngle: %20s (%14f°)" % ( repr( randomAngle ), randomDegree ) )

                x_old[0] = x[0]
                y_old[0] = y[0]
                x[0]     += math.cos( randomAngle )
                y[0]     += math.sin( randomAngle )

                addLine()

            if isToUpdateProgressBar:
                return progressBar.incrementBarParcial()

        # Do the iterations
        for index in xrange( 0, totalFullIterations ):

            if iterationFullStep( MINIMUM_STEPS_TO_SHOW_PARTIAL_PROGRESS ):
                return True

        # Compute the remaining steps
        iterationFullStep( lastInterations )

        if isToDrawLines:
            self.drawingPanel.addExampleEllipse( x[0], y[0] )

        pathLength = self.getPointsDistance( 0, x[0], 0, y[0] )
        self.allIterationsResult.append( pathLength )

        # log( 2, "( Simulator::runOneIteration ) x: %14f, y: %14f, pathLength: %14f" % ( x[0], y[0], pathLength ) )
        return False

    def getRandomAngle( self ):
        return random.uniform(0, self.maxAngle)

    def getPointsDistance( self, x1, y1, x2, y2 ):
        return math.hypot( x2 - x1, y2 - y1 )



