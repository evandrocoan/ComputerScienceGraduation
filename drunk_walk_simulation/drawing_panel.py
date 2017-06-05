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
from settings import *

from PyQt4 import QtGui, QtCore
log( 1, "Importing " + __name__ )


class DrawingPanel( QtGui.QGraphicsView ):

    def __init__( self, parent=None ):
        super( DrawingPanel, self ).__init__( parent )
        self.isScrollEnabled = False
        self.paintAmplifation = 50

        self.currentAxisX = None
        self.currentAxisY = None

        self.scene = QtGui.QGraphicsScene()
        self.setScene( self.scene )

        self.clearView()
        self.configurePanelSettings()

        self.drawAxes( 200 )
        self.brush = QtGui.QBrush( QtGui.QColor( 125, 125, 125, 125 ) )

        self.pencil = QtGui.QPen( QtCore.Qt.black, 1 )
        self.pencil.setStyle( QtCore.Qt.SolidLine )

        # self.addExampleLine()
        # self.addExampleEllipse()

    def clearView( self ):
        """
            Remove all objects from the drawing panel and recreate its origin axis.
        """
        self.scene.clear()
        self.scene = QtGui.QGraphicsScene()

        self.setScene( self.scene )
        self.setDefealtZoom()

    def setDefealtZoom( self ):
        self.setTransform( QtGui.QTransform() )

    def configurePanelSettings( self ):
        # http://pyqt.sourceforge.net/Docs/PyQt4/qgraphicsview.html#DragMode-enum
        # https://stackoverflow.com/questions/40684884/how-can-i-override-a-pyqt4-qgraphicsview-mousepressevent
        self.setDragMode( QtGui.QGraphicsView.ScrollHandDrag )

        # http://pyqt.sourceforge.net/Docs/PyQt4/qpainter.html#RenderHint-enum
        self.setRenderHint( QtGui.QPainter.Antialiasing );
        self.setRenderHint( QtGui.QPainter.TextAntialiasing );
        self.setRenderHint( QtGui.QPainter.SmoothPixmapTransform );
        self.setRenderHint( QtGui.QPainter.HighQualityAntialiasing );

    def fitAxes( self ):
        itemsBounding = self.scene.itemsBoundingRect()
        width  = itemsBounding.width()
        height = itemsBounding.height()

        upPoint   = itemsBounding.topLeft()
        downPoint = itemsBounding.bottomRight()

        log( 4, "( DrawingPanel::fitAxes ) width :        %s" % ( str( width ) ) )
        log( 4, "( DrawingPanel::fitAxes ) height:        %s" % ( str( height ) ) )
        log( 4, "( DrawingPanel::fitAxes ) upPoint:       %s" % ( str( upPoint ) ) )
        log( 4, "( DrawingPanel::fitAxes ) downPoint:     %s" % ( str( downPoint ) ) )
        log( 4, "( DrawingPanel::fitAxes ) itemsBounding: %s" % ( str( itemsBounding ) ) )

        pencil = QtGui.QPen( QtCore.Qt.blue, 2)
        pencil.setStyle( QtCore.Qt.DotLine )

        yUp   = int( upPoint.y() )
        yDown = int( downPoint.y() )
        xUp   = int( upPoint.x() )
        xDown = int( downPoint.x() )

        yUp   = self.increaseAxe( yUp  , yDown )
        yDown = self.increaseAxe( yDown, yUp   )
        xUp   = self.increaseAxe( xUp  , xDown )
        xDown = self.increaseAxe( xDown, xUp   )

        log( 4, "( DrawingPanel::fitAxes ) yUp :      %s" % ( str( yUp ) ) )
        log( 4, "( DrawingPanel::fitAxes ) yDown:     %s" % ( str( yDown ) ) )
        log( 4, "( DrawingPanel::fitAxes ) xUp:       %s" % ( str( xUp ) ) )
        log( 4, "( DrawingPanel::fitAxes ) xDown:     %s" % ( str( xDown ) ) )

        self.currentAxisY = self.scene.addLine( QtCore.QLineF( 0, yUp, 0, yDown ), pencil )
        self.currentAxisX = self.scene.addLine( QtCore.QLineF( xUp, 0, xDown, 0 ), pencil )

        self.fitAxisLables( yUp, yDown, xUp, xDown )
        return itemsBounding

    def increaseAxe( self, increasePoint, oppositePoint ):
        extraScreen = 200

        if increasePoint < 0:

            if oppositePoint >= 0:
                return increasePoint - extraScreen

            else:
                return increasePoint + extraScreen

        if oppositePoint < 0:
            return increasePoint + extraScreen

        return increasePoint - extraScreen

    def scaleAxeLabel( self, point ):
        return abs( int( point / self.paintAmplifation / 2 ) )

    def fitAxisLables( self, yUp, yDown, xUp, xDown ):
        labelSize   = 2
        labelShiftX = 0
        labelShiftY = - self.paintAmplifation * .35

        log( 4, "( DrawingPanel::addAxisLables ) labelSize:   %f" % ( labelSize ) )
        log( 4, "( DrawingPanel::addAxisLables ) labelShiftX: %f, labelShiftY: %f" % ( labelShiftX, labelShiftY ) )

        yUp   = self.scaleAxeLabel( yUp   )
        yDown = self.scaleAxeLabel( yDown )
        xUp   = self.scaleAxeLabel( xUp   )
        xDown = self.scaleAxeLabel( xDown )

        log( 4, "( DrawingPanel::fitAxisLables ) yUp :  %s" % ( str( yUp ) ) )
        log( 4, "( DrawingPanel::fitAxisLables ) yDown: %s" % ( str( yDown ) ) )
        log( 4, "( DrawingPanel::fitAxisLables ) xUp:   %s" % ( str( xUp ) ) )
        log( 4, "( DrawingPanel::fitAxisLables ) xDown: %s" % ( str( xDown ) ) )

        for label_index in range( 1, xDown + 1 ):
            labelValue    = label_index * labelSize
            labelPosition = labelValue * self.paintAmplifation

            labelItem = self.scene.addText( str( labelValue ) )
            labelItem.setPos( labelPosition - self.paintAmplifation * .4, labelShiftX )

        for label_index in range( 1, xUp ):
            labelValue    = - label_index * labelSize
            labelPosition = labelValue * self.paintAmplifation

            labelItem = self.scene.addText( str( labelValue ) )
            labelItem.setPos( labelPosition, labelShiftX )

        for label_index in range( 1, yUp ):
            labelValue    = - label_index * labelSize
            labelPosition = labelValue * self.paintAmplifation

            labelItem = self.scene.addText( str( labelValue ) )
            labelItem.setPos( labelShiftY, labelPosition + self.paintAmplifation * .05 )

        for label_index in range( 1, yDown + 1 ):
            labelValue    = - label_index * labelSize
            labelPosition = labelValue * self.paintAmplifation

            labelItem = self.scene.addText( str( labelValue ) )
            labelItem.setPos( labelShiftY, -labelPosition - self.paintAmplifation * .4 )

    def drawAxes(self, length ):
        self.addAxisLines( length )
        self.addAxisLables( length )

    def addAxisLines( self, length ):
        pencil = QtGui.QPen( QtCore.Qt.blue, 2)
        pencil.setStyle( QtCore.Qt.DotLine )

        self.currentAxisX = self.scene.addLine( QtCore.QLineF( 0, -length, 0, length ), pencil )
        self.currentAxisY = self.scene.addLine( QtCore.QLineF( -length, 0, length, 0 ), pencil )

    def addAxisLables( self, length ):
        labelSize   = 2
        labelShiftX = 0
        labelShiftY = - self.paintAmplifation * .35
        labelsCount = int( length / self.paintAmplifation / labelSize + 1 )

        log( 4, "( DrawingPanel::addAxisLables ) labelSize:   %f, labelsCount: %f" % ( labelSize  , labelsCount ) )
        log( 4, "( DrawingPanel::addAxisLables ) labelShiftX: %f, labelShiftY: %f" % ( labelShiftX, labelShiftY ) )

        for label_index in range( 1, labelsCount ):
            labelValue    = label_index * labelSize
            labelPosition = labelValue * self.paintAmplifation

            # X axis
            labelItem = self.scene.addText( str( labelValue ) )
            labelItem.setPos( labelPosition - self.paintAmplifation * .4, labelShiftX )

            labelItem = self.scene.addText( str( -labelValue ) )
            labelItem.setPos( -labelPosition, labelShiftX )

            # Y axis
            labelItem = self.scene.addText( str( labelValue ) )
            labelItem.setPos( labelShiftY, labelPosition - self.paintAmplifation * .4 )

            labelItem = self.scene.addText( str( labelValue ) )
            labelItem.setPos( labelShiftY, -labelPosition )

    def drawLine( self, x1, y1, x2, y2 ):
        """
            Drawing an arrow
            https://math.stackexchange.com/questions/1314006/drawing-an-arrow

            Degree/Radian Circle
            http://math.rice.edu/~pcmi/sphere/drg_txt.html

            Python Numbers
            https://www.tutorialspoint.com/python/python_numbers.htm
        """
        angle = math.pi / 6
        arraw_size = 0.2

        x3 = x2 + arraw_size * ( (x1 - x2)*math.cos(angle) + (y1 - y2)*math.sin(angle) )
        y3 = y2 + arraw_size * ( (y1 - y2)*math.cos(angle) - (x1 - x2)*math.sin(angle) )

        x4 = x2 + arraw_size * ( (x1 - x2)*math.cos(angle) - (y1 - y2)*math.sin(angle) )
        y4 = y2 + arraw_size * ( (y1 - y2)*math.cos(angle) + (x1 - x2)*math.sin(angle) )

        self.scene.addPolygon( QtGui.QPolygonF( [ \
                QtCore.QPointF( self.paintAmplifation * x3 , self.paintAmplifation * y3 ), \
                QtCore.QPointF( self.paintAmplifation * x2, self.paintAmplifation * y2 ), \
                QtCore.QPointF( self.paintAmplifation * x4, self.paintAmplifation * y4 ) ] ), self.pencil, self.brush )

        self.scene.addLine( QtCore.QLineF( \
                self.paintAmplifation * x1, self.paintAmplifation * y1, \
                self.paintAmplifation * x2, self.paintAmplifation * y2 ), self.pencil )

    def addExampleLine( self ):
        """
            QPen Class Reference
            http://pyqt.sourceforge.net/Docs/PyQt4/qpen.html
        """
        pencil = QtGui.QPen( QtCore.Qt.black, 2 )
        pencil.setStyle( QtCore.Qt.SolidLine )

        # How to rotate a polygon on a QGraphicsScene at pyqt4?
        # https://stackoverflow.com/questions/44267547/how-to-rotate-a-polygon-on-a-qgraphicsscene-at-pyqt4
        polygonItem = self.scene.addLine( QtCore.QLineF( 0, 0, 300, 900 ), pencil )

        transform = QtGui.QTransform()
        transform.translate( -150, -450 )

        polygonItem.setTransform( transform )

    def addExampleEllipse(self):
        """
            Python PyQt: How can I move my widgets on the window with mouse?
            https://stackoverflow.com/questions/12213391/python-pyqt-how-can-i-move-my-widgets-on-the-window-with-mouse
        """
        x = 0
        y = 0
        w = 45
        h = 45
        pen   = QtGui.QPen( QtGui.QColor( QtCore.Qt.green ) )
        brush = QtGui.QBrush( pen.color().darker( 150 ) )

        item = self.scene.addEllipse( x, y, w, h, pen, brush )
        item.setFlag( QtGui.QGraphicsItem.ItemIsMovable )

    def wheelEvent( self, event ):
        """
            PyQT4 WheelEvent? how to detect if the wheel have been use?
            https://stackoverflow.com/questions/9475772/pyqt4-wheelevent-how-to-detect-if-the-wheel-have-been-use

            QGraphicsView Zooming in and out under mouse position using mouse wheel
            https://stackoverflow.com/questions/19113532/qgraphicsview-zooming-in-and-out-under-mouse-position-using-mouse-wheel
        """

        if self.isScrollEnabled:
            super( DrawingPanel, self ).wheelEvent( event )

        else:
            self.performScrollZoom( event )

    def performScrollZoom( self, event ):
        # Zoom Factor
        zoomInFactor = 1.1
        zoomOutFactor = 1 / zoomInFactor

        # Set Anchors
        self.setTransformationAnchor( QtGui.QGraphicsView.NoAnchor )
        self.setResizeAnchor( QtGui.QGraphicsView.NoAnchor )

        # Save the scene pos
        oldPos = self.mapToScene( event.pos() )

        # Zoom
        if event.delta() > 0:
            zoomFactor = zoomInFactor

        else:
            zoomFactor = zoomOutFactor

        self.scale( zoomFactor, zoomFactor )

        # Get the new position
        newPos = self.mapToScene( event.pos() )

        # Move scene to old position
        delta = newPos - oldPos
        self.translate( delta.x(), delta.y() )



