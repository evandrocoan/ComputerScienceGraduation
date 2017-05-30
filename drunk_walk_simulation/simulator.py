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

from settings import *
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
        pass

    def startSimulation( self ):
        """
            Begin the simulation process.
        """
        pass
