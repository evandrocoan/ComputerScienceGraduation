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
        self.maxAngle     = 2 * math.pi
        self.pathEndPoint = QtCore.QPointF( 0.0, 0.0 )

        self.allIterationsResult = []
        self.firstIterationSteps = []

        self.mainWindow   = mainWindow
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
        howManySteps  = int( self.mainWindow.stepNumberLineEdit.text() )
        howManyTimes  = int( self.mainWindow.replicationsNumberLineEdit.text() )
        isToDrawLines = howManyTimes < 2

        lastIterations      = howManySteps % MINIMUM_STEPS_TO_SHOW_PARTIAL_PROGRESS
        totalFullIterations = int( howManySteps / MINIMUM_STEPS_TO_SHOW_PARTIAL_PROGRESS )
        totalIterations     = totalFullIterations + ( 1 if lastIterations > 0 else 0 )

        # if the interaction step is too big, the application will hang
        timeStepSize = 1

        if isToDrawLines:
            self.mainWindow.handleClearView( True )
            timeStepSize = MINIMUM_STEPS_TO_SHOW_PARTIAL_PROGRESS

        else:
            self.mainWindow.handleClearView()

            if howManySteps <= int( MINIMUM_STEPS_TO_SHOW_PARTIAL_PROGRESS / 1000 ):
                timeStepSize = int( MINIMUM_STEPS_TO_SHOW_PARTIAL_PROGRESS / 100 )

            elif howManySteps <= int( MINIMUM_STEPS_TO_SHOW_PARTIAL_PROGRESS / 100 ):
                timeStepSize = int( MINIMUM_STEPS_TO_SHOW_PARTIAL_PROGRESS / 1000 )

            elif howManySteps <= int( MINIMUM_STEPS_TO_SHOW_PARTIAL_PROGRESS / 10 ):
                timeStepSize = int( MINIMUM_STEPS_TO_SHOW_PARTIAL_PROGRESS / 10000 )

        lastCycles      = howManyTimes % timeStepSize
        totalFullCycles = int( howManyTimes / timeStepSize )
        totalCycles     = totalFullCycles + ( 1 if lastCycles > 0 else 0 )

        progressBar = ProgressBar( None, totalCycles, totalIterations, howManyTimes )
        progressBar.show()

        # Set it 100% it will not be updated for performance increase
        if not isToDrawLines \
                and howManySteps <= MINIMUM_STEPS_TO_SHOW_PARTIAL_PROGRESS:

            # Bug fix. Directly setting it to `totalIterations` was causing it to not fill completely
            progressBar.progressBarPartial.setMaximum( 5 )
            QtGui.qApp.processEvents()
            progressBar.progressBarPartial.setValue( 5 )

        log( 2, "( Simulator::startSimulation_ ) lastCycles:      %d" % ( lastCycles ) )
        log( 2, "( Simulator::startSimulation_ ) timeStepSize:    %d" % ( timeStepSize ) )
        log( 2, "( Simulator::startSimulation_ ) totalFullCycles: %d" % ( totalFullCycles ) )
        log( 2, "( Simulator::startSimulation_ ) totalCycles:     %d" % ( totalCycles ) )

        # Why is local variable access faster than class member access in Python?
        # https://stackoverflow.com/questions/12397984/why-is-local-variable-access-faster-than-class-member-access-in-python
        #
        # My Python for loop is causing a MemoryError. How can I optimize this?
        # https://stackoverflow.com/questions/4405083/my-python-for-loop-is-causing-a-memoryerror-how-can-i-optimize-this
        def iterationFullCycle( howManyCycles ):

            for iterationCount in xrange( 0, howManyCycles ):

                # Stops the process when the cancel button is hit
                if self.runOneIteration( isToDrawLines, totalFullIterations, lastIterations, progressBar ):
                    return True

            return progressBar.incrementBarOverall( timeStepSize )

        # Do the cycles
        for index in xrange( 0, totalFullCycles ):

            if iterationFullCycle( timeStepSize ):
                break


        # Compute the remaining cycles, only if not cancelled by the user
        if progressBar._active:
            iterationFullCycle( lastCycles )

        if isToDrawLines:
            itemsBounding = self.drawingPanel.fitAxes()

        # Need to re-check due iterations only with one cycle
        if progressBar._active:

            if isToDrawLines:
                self.mainWindow.fitSceneInView( itemsBounding )

                self.showResults( itemsBounding, howManySteps )
                self.plotWalkedPath()

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

    def plotWalkedPath( self ):
        """
            Plotting more than one plot on the same set of axes
            http://www.ast.uct.ac.za/~sarblyth/pythonGuide/PythonPlottingBeginnersGuide.pdf

            Matplotlib: linewidth is added to the length of a line
            https://stackoverflow.com/questions/10297220/matplotlib-linewidth-is-added-to-the-length-of-a-line
        """

        # This is only set when the simulation was cancelled by the user
        howManySteps = len( self.firstIterationSteps)

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

    def runOneIteration( self, isToDrawLines, totalFullIterations, lastIterations, progressBar ):
        """
            Initializing a list to a known number of elements in Python [duplicate]
            https://stackoverflow.com/questions/521674/initializing-a-list-to-a-known-number-of-elements-in-python
        """
        returnValue = False

        x     = [0.0]
        y     = [0.0]
        x_old = [0.0]
        y_old = [0.0]

        pathLength            = 0.0
        stepsPerCycle         = 0
        isToUpdateProgressBar = totalFullIterations > 0

        log( 2, "( Simulator::runOneIteration ) isToDrawLines:       %d" % ( isToDrawLines ) )
        log( 2, "( Simulator::runOneIteration ) lastIterations:      %d" % ( lastIterations ) )
        log( 2, "( Simulator::runOneIteration ) totalFullIterations: %d" % ( totalFullIterations ) )

        def computeLineWithHistogram():
            pathLength = self.getPointsDistance( 0, x[0], 0, y[0] )
            self.firstIterationSteps.append( pathLength )

        if isToDrawLines:
            stepsPerCycle = MINIMUM_STEPS_WHEN_DRAWING_THE_PATH

            # Simplify if the requested simulation is too big
            if MINIMUM_STEPS_TO_SHOW_PARTIAL_PROGRESS * totalFullIterations > MAXIMUM_COMPUTABLE_SIZE:

                def addLine():
                    self.drawingPanel.drawSimpleLine( x_old[0], y_old[0], x[0], y[0] )

            else:

                def addLine():
                    computeLineWithHistogram()
                    self.drawingPanel.drawLine( x_old[0], y_old[0], x[0], y[0] )

        else:
            stepsPerCycle = MINIMUM_STEPS_TO_SHOW_PARTIAL_PROGRESS

            def addLine():
                pass

        # scoping error in recursive closure
        # https://stackoverflow.com/questions/2516652/scoping-error-in-recursive-closure
        def iterationFullStep( howManySteps ):
            # log( 2, "( Simulator::runOneIteration ) howManySteps: %d" % ( howManySteps) )

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

            if iterationFullStep( stepsPerCycle ):
                returnValue = True
                break

        # Compute the remaining steps
        iterationFullStep( lastIterations )
        self.pathEndPoint = ( x[0], y[0] )

        if isToDrawLines:
            self.drawingPanel.addExampleEllipse( x[0], y[0] )

        pathLength = self.getPointsDistance( 0, x[0], 0, y[0] )
        self.allIterationsResult.append( pathLength )

        # log( 2, "( Simulator::runOneIteration ) self.pathEndPoint: %s" % ( str( self.pathEndPoint ) ) )
        # log( 2, "( Simulator::runOneIteration ) x: %14f, y: %14f, pathLength: %14f" % ( x[0], y[0], pathLength ) )
        return returnValue

    def getRandomAngle( self ):
        return random.uniform(0, self.maxAngle)

    def getPointsDistance( self, x1, y1, x2, y2 ):
        return math.hypot( x2 - x1, y2 - y1 )



