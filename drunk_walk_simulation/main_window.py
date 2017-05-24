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
log( 1, "Importing " + __name__ )


class MainWindow(QWidget):

    def __init__(self):
        """
            The QWidget widget is the base class of all user interface objects in PyQt4.

            The widget is the atom of the user interface: it receives mouse, keyboard and other
            events from the window system, and paints a representation of itself on the screen.
            Every widget is rectangular, and they are sorted in a Z-order. A widget is clipped by
            its parent and by the widgets in front of it.

            http://pyqt.sourceforge.net/Docs/PyQt4/qwidget.html
        """
        super(MainWindow, self).__init__()
        self.createAndDisplayWindow()

    def createAndDisplayWindow(self):

        # Set window size.
        self.resize( 1120, 640 )

        # Set window title
        self.setWindowTitle( "Drunk Walk Simulator" )

        # https://github.com/GNOME/adwaita-icon-theme
        # https://code.google.com/archive/p/faenza-icon-theme/
        mainApplicationIcon = QIcon( 'login.png' )

        # PyQt4 set windows taskbar icon
        # https://stackoverflow.com/questions/12432637/pyqt4-set-windows-taskbar-icon
        # https://stackoverflow.com/questions/44161669/how-to-set-a-python-qt4-window-icon
        self.setWindowIcon( mainApplicationIcon )

        # Show window
        self.show()





