# -*- coding: UTF-8 -*-

"""

Python PyQt4.QtGui.QMessageBox Examples
http://www.programcreek.com/python/example/62361/PyQt4.QtGui.QMessageBox

How to display a message box on PyQT4?
https://stackoverflow.com/questions/4155052/how-to-display-a-message-box-on-pyqt4

"""

from PyQt4 import QtGui, QtCore


class Window( QtGui.QWidget ):

    def __init__( self ):
        QtGui.QWidget.__init__( self )

        msgBox = QtGui.QMessageBox()
        msgBox.setIcon( QtGui.QMessageBox.Information )
        msgBox.setText( "Do not stare into laser with remaining eye" )

        msgBox.setInformativeText( "Do you really want to disable safety enforcement?" )
        msgBox.addButton( QtGui.QMessageBox.Yes )
        msgBox.addButton( QtGui.QMessageBox.No )

        msgBox.setDefaultButton( QtGui.QMessageBox.No )
        returnValue = msgBox.exec_()

        if returnValue == QtGui.QMessageBox.Yes:
            print( "Yes" )
            return
        else:
            print( "No" )
            return

if __name__ == '__main__':

    import sys
    app = QtGui.QApplication( sys.argv )
    window = Window()
    # window.show()
    sys.exit( app.exec_() )




