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

import sys
import time

from settings import *
from PyQt4 import QtGui, QtCore, Qt

log( 1, "Importing " + __name__ )


class ProgressBar( QtGui.QWidget ):
    """

    Python QT ProgressBar
    https://stackoverflow.com/questions/13269936/python-qt-progressbar

    """
    def __init__( self, parent=None, total=20, partial=20, timeStepSize=1 ):
        super( ProgressBar, self ).__init__( parent )
        self.resize( 720, 100 )

        self.progressBarOverall = QtGui.QProgressBar()
        self.progressBarOverall.setMinimum( 1 )
        self.progressBarOverall.setMaximum( total )

        self.progressBarPartial = QtGui.QProgressBar()
        self.progressBarPartial.setMinimum( 1 )
        self.progressBarPartial.setMaximum( partial )

        self.button = QtGui.QPushButton( 'Stop Simulation' )
        self.button.clicked.connect( self.handleButton )

        main_layout = QtGui.QGridLayout()
        main_layout.addWidget( self.button, 0, 0 )
        main_layout.addWidget( self.progressBarPartial, 0, 1 )
        main_layout.addWidget( self.progressBarOverall, 1, 1 )

        self.processedStepsTotal   = QtGui.QLabel( 'of interactions ' + intWithCommas( timeStepSize ) )
        self.processedStepsOverall = QtGui.QLabel( '0' )

        horizontalLayout = QtGui.QHBoxLayout()
        horizontalLayout.addWidget( self.processedStepsOverall )
        horizontalLayout.addWidget( self.processedStepsTotal )

        main_layout.addLayout( horizontalLayout, 2, 1, QtCore.Qt.AlignRight )

        self.setLayout( main_layout )
        self.setWindowTitle('Progress')

        self._active = True

    def handleButton( self ):
        self._active = False
        self.hide()

    def closeEvent( self, event ):
        self._active = False
        self.hide()

    def incrementBarParcial( self ):
        """
            Return true when the simulation was cancelled by the user, False otherwise.
        """
        value = self.progressBarPartial.value() + 1

        if not self._active or \
                value > self.progressBarPartial.maximum():

            self.progressBarPartial.reset()
            return not self._active

        self.progressBarPartial.setValue( value )
        QtGui.qApp.processEvents()

        return not self._active

    def incrementBarOverall( self, timeStepSize=1 ):
        """
            Return true when the simulation was cancelled by the user, False otherwise.
        """
        value = self.progressBarOverall.value() + 1

        if not self._active or \
                value >= self.progressBarOverall.maximum():

            self.hide()
            return True

        self.processedStepsOverall.setText( str( value * timeStepSize ) )
        self.progressBarOverall.setValue( value )
        QtGui.qApp.processEvents()

        return False

