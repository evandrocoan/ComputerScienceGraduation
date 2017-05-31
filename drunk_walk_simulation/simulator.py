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
import random

from settings      import *
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
        self.mainWindow = mainWindow
        self.drawingPanel = drawingPanel

        log( 2, "( Simulator::__init__ ) self.maxAngle: " + repr( self.maxAngle ) )

    def startSimulation( self ):
        """
            Begin the simulation process.
        """
        howManySteps = int( self.mainWindow.stepNumberLineEdit.text() )
        howManyTimes = int( self.mainWindow.replicationsNumberLineEdit.text() )

        x = 0.0
        y = 0.0
        pathLength = 0.0

        for index in range( 0, howManySteps ):
            randomAngle = self.getRandomAngle()
            randomDegree = math.degrees( randomAngle )
            # log( 2, "( Simulator::startSimulation ) randomAngle: %s (%f°)" % ( repr( randomAngle ), randomDegree ) )

            x_old = x
            y_old = y
            x += math.cos( randomAngle )
            y += math.sin( randomAngle )

            self.drawingPanel.drawLines( x_old, y_old, x, y )
            pathLength = self.getPointsDistance( 0, x, 0, y )

            # log( 2, "( Simulator::startSimulation ) x: %f, y: %f, pathLength: %f" % ( x, y, pathLength ) )

        log( 2, "( Simulator::startSimulation ) x: %f, y: %f, pathLength: %f" % ( x, y, pathLength ) )

    def getRandomAngle( self ):
        return random.uniform(0, self.maxAngle)

    def getPointsDistance( self, x1, y1, x2, y2 ):
        return math.hypot( x2 - x1, y2 - y1 )
