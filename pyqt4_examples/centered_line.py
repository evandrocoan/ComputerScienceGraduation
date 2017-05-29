from PyQt4 import QtGui, QtCore

class MyFrame(QtGui.QGraphicsView):
    """
        Python PyQt: How can I move my widgets on the window with mouse?
        https://stackoverflow.com/questions/12213391/python-pyqt-how-can-i-move-my-widgets-on-the-window-with-mouse
    """
    def __init__( self, parent = None ):
        super(MyFrame, self).__init__(parent)

        scene = QtGui.QGraphicsScene()
        self.setScene(scene)
        self.resize( 400, 240 )

        # http://pyqt.sourceforge.net/Docs/PyQt4/qpen.html
        pencil = QtGui.QPen( QtCore.Qt.black, 2)
        pencil.setStyle( QtCore.Qt.SolidLine )

        # pencil.setStyle( QtCore.Qt.UpArrow )
        scene.addLine( QtCore.QLineF( 0, 0, 100, 100 ), pencil )

if ( __name__ == '__main__' ):
    app = QtGui.QApplication([])
    f = MyFrame()
    f.show()
    app.exec_()
