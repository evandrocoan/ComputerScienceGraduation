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

from settings      import *
from simulator     import Simulator
from drawing_panel import DrawingPanel

from PyQt4 import QtGui, QtCore, Qt
log( 1, "Importing " + __name__ )


class MainWindow( QtGui.QWidget ):

    def __init__( self ):
        """
            The QWidget widget is the base class of all user interface objects in PyQt4.

            The widget is the atom of the user interface: it receives mouse, keyboard and other
            events from the window system, and paints a representation of itself on the screen.
            Every widget is rectangular, and they are sorted in a Z-order. A widget is clipped by
            its parent and by the widgets in front of it.

            http://pyqt.sourceforge.net/Docs/PyQt4/qwidget.html
        """
        super( MainWindow, self ).__init__()
        self.simulation = None

        # Drawing a line consisting of multiple points using PyQt
        # https://stackoverflow.com/questions/13368947/drawing-a-line-consisting-of-multiple-points-using-pyqt
        self.drawingPanel = DrawingPanel( self )

        self.addButtons()
        self.setWindowLayout()
        self.configureMainWindow()

    def configureMainWindow( self ):
        # Set window size.
        self.resize( 1120, 640 )

        # Set window title
        self.setWindowTitle( "Drunk Walk Simulator" )

        # https://github.com/GNOME/adwaita-icon-theme
        # https://code.google.com/archive/p/faenza-icon-theme/
        mainApplicationIcon = QtGui.QIcon( 'login.png' )

        # PyQt4 set windows taskbar icon
        # https://stackoverflow.com/questions/12432637/pyqt4-set-windows-taskbar-icon
        # https://stackoverflow.com/questions/44161669/how-to-set-a-python-qt4-window-icon
        self.setWindowIcon( mainApplicationIcon )

    def addButtons( self ):
        self.startSimulationButton = QtGui.QPushButton( 'Start Simulation', self )
        self.startSimulationButton.clicked.connect( self.handleSimulationStart )

        self.stepNumberLineLabel = QtGui.QLabel( 'How many steps?' )
        self.stepNumberLineEdit = QtGui.QLineEdit()
        self.stepNumberLineEdit.setValidator( QtGui.QIntValidator( 1, 999999, self ) )
        self.stepNumberLineEdit.setText( "1000" )

        self.replicationsNumberLineLabel = QtGui.QLabel( 'How many Replications?' )
        self.replicationsNumberLineEdit = QtGui.QLineEdit()
        self.replicationsNumberLineEdit.setValidator( QtGui.QIntValidator( 1, 999999, self ) )
        self.replicationsNumberLineEdit.setText( "1" )

        # Creates the clear button
        self.clearDrawingButton = QtGui.QPushButton( 'Clear Drawing Panel', self )
        self.clearDrawingButton.clicked.connect( self.handleClearView )

        # Creates the restore zoom button
        # https://forum.qt.io/topic/14842/solved-how-to-fit-in-view-the-pixmaps-in-qgraphicsview-qgraphicsscene-without-changing-aspect-ratio
        self.fitInViewButton = QtGui.QPushButton( 'Fit In View', self )
        self.fitInViewButton.clicked.connect( self.handleFitInView )

        # Programmatically Toggle a Python PyQt QPushbutton
        # https://stackoverflow.com/questions/19508450/programmatically-toggle-a-python-pyqt-qpushbutton
        self.zoomButton = QtGui.QPushButton( 'Use zoom?', self )
        self.zoomButton.clicked.connect( self.handleZoomButton )
        self.zoomButton.setCheckable( True )
        self.zoomButton.setChecked(True)

    def setWindowLayout( self ):
        # How to align the layouts QHBoxLayout and QVBoxLayout on pyqt4?
        # https://stackoverflow.com/questions/44230856/how-to-align-the-layouts-qhboxlayout-and-qvboxlayout-on-pyqt4
        horizontalLayout = QtGui.QHBoxLayout()
        horizontalLayout.addWidget( self.startSimulationButton )

        horizontalLayout.addWidget( self.replicationsNumberLineLabel )
        horizontalLayout.addWidget( self.replicationsNumberLineEdit )

        horizontalLayout.addWidget( self.stepNumberLineLabel )
        horizontalLayout.addWidget( self.stepNumberLineEdit )

        horizontalLayout.addWidget( self.clearDrawingButton )
        horizontalLayout.addWidget( self.zoomButton )
        horizontalLayout.addWidget( self.fitInViewButton )

        # Creates a box to align vertically the panels
        # https://doc.qt.io/qt-4.8/qvboxlayout.html
        #
        # Review example
        # http://zetcode.com/gui/pyqt4/layoutmanagement/
        verticalLayout = QtGui.QVBoxLayout( self )
        verticalLayout.addLayout( horizontalLayout )
        verticalLayout.addWidget( self.drawingPanel )

        self.setLayout( verticalLayout )

    def handleClearView( self ):
        # How to restore a QGraphicsView zoom to 100%, i.e., the zoom when the program started?
        # https://stackoverflow.com/questions/44270301/how-to-restore-a-qgraphicsview-zoom-to-100-i-e-the-zoom-when-the-program-sta
        self.drawingPanel.clearView()
        self.drawingPanel.setTransform( QtGui.QTransform() )

    def handleSimulationStart( self ):

        try:
            self.simulation = Simulator( self, self.drawingPanel )

        except:
            print( "Error" )
            return

        self.handleClearView()
        self.simulation.startSimulation()

    def handleFitInView( self ):
        # Auto scale a QGraphicsView
        # http://www.qtcentre.org/threads/42917-Auto-scale-a-QGraphicsView
        self.drawingPanel.ensureVisible ( self.drawingPanel.scene.itemsBoundingRect() )
        self.drawingPanel.fitInView( self.drawingPanel.scene.itemsBoundingRect(), QtCore.Qt.KeepAspectRatio )

    def handleZoomButton( self ):

        if self.drawingPanel.isScrollEnabled:
            self.zoomButton.setChecked(True)
            self.drawingPanel.isScrollEnabled = False

        else:
            self.zoomButton.setChecked(False)
            self.drawingPanel.isScrollEnabled = True



