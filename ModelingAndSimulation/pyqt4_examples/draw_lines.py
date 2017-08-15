from PyQt4 import QtGui, QtCore

class Window( QtGui.QWidget ):
    """
        ZetCode PyQt4 tutorial

        This example draws 9 rectangles in different brush styles.

        author: Jan Bodnar
        website: zetcode.com
        last edited: September 2011
        http://zetcode.com/gui/pyqt4/drawing/


        How to implement a simple button in PyQt
        https://stackoverflow.com/questions/8762870/how-to-implement-a-simple-button-in-pyqt
    """
    def __init__( self ):
        QtGui.QWidget.__init__( self )
        self.button = QtGui.QPushButton( 'Test', self )
        self.button.clicked.connect( self.handleButton )
        layout = QtGui.QVBoxLayout( self )
        layout.addWidget( self.button )
        self.resize( 500, 640 )

    def handleButton( self ):
        print ( 'Hello World' )

    def paintEvent( self, e ):
        painter = QtGui.QPainter()
        painter.begin( self )
        self.drawLines( painter )
        painter.end()

    def drawLines( self, painter ):
        pencil = QtGui.QPen( QtCore.Qt.black, 2, QtCore.Qt.SolidLine )

        painter.setPen( pencil )
        painter.drawLine( 1, 1, 250, 40 )

        painter.setPen( pencil )
        painter.drawLine( 20, 40, 250, 40 )

        pencil.setStyle( QtCore.Qt.DashLine )
        painter.setPen( pencil )
        painter.drawLine( 20, 80, 250, 80 )

        pencil.setStyle( QtCore.Qt.DashDotLine )
        painter.setPen( pencil )
        painter.drawLine( 20, 120, 250, 120 )

        pencil.setStyle( QtCore.Qt.DotLine )
        painter.setPen( pencil )
        painter.drawLine( 20, 160, 250, 160 )

        pencil.setStyle( QtCore.Qt.DashDotDotLine )
        painter.setPen( pencil )
        painter.drawLine( 20, 200, 250, 200 )

        pencil.setStyle( QtCore.Qt.CustomDashLine )
        pencil.setDashPattern( [ 1, 4, 5, 4 ] )
        painter.setPen( pencil )
        painter.drawLine( 20, 240, 250, 240 )

if __name__ == '__main__':

    import sys
    app = QtGui.QApplication( sys.argv )
    window = Window()
    window.show()
    sys.exit( app.exec_() )
