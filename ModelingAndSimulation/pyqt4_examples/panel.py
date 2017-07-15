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
        self.resize( 200, 240 )

        # add some items
        x = 0
        y = 0
        w = 45
        h = 45
        pen   = QtGui.QPen(QtGui.QColor(QtCore.Qt.green))
        brush = QtGui.QBrush(pen.color().darker(150))

        item = scene.addEllipse(x, y, w, h, pen, brush)
        item.setFlag(QtGui.QGraphicsItem.ItemIsMovable)

        # create an open path
        path = QtGui.QPainterPath()
        path.moveTo(-w, -h)
        path.lineTo(-w, h)
        path.lineTo(w, h)
        path.lineTo(w, -h)

        clr   = QtGui.QColor('blue')
        clr.setAlpha(120)
        brush = QtGui.QBrush(clr)
        pen   = QtGui.QPen(QtCore.Qt.NoPen)
        fill_item = scene.addRect(-w, y, w*2, h, pen, brush)
        path_item = scene.addPath(path)

if ( __name__ == '__main__' ):
    app = QtGui.QApplication([])
    f = MyFrame()
    f.show()
    app.exec_()
