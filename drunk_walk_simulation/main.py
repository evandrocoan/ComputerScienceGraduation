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
sys.path.insert(0,'../python_debug')

from debug_tools import log

from settings import *
from PyQt4.QtGui import *

log( 1, "Debugging" )
log( 1, "..." )
log( 1, "..." )


def main():

    # Create an PyQT4 application object.
    #
    # QApplication contains the main event loop, where all events from the window system and other
    # sources are processed and dispatched. It also handles the application's initialization,
    # finalization, and provides session management. In addition, QApplication handles most of the
    # system-wide and application-wide settings.
    # http://pyqt.sourceforge.net/Docs/PyQt4/qapplication.html
    simulatorApplication = QApplication( sys.argv )

    # The QWidget widget is the base class of all user interface objects in PyQt4.
    #
    # The widget is the atom of the user interface: it receives mouse, keyboard and other events
    # from the window system, and paints a representation of itself on the screen. Every widget is
    # rectangular, and they are sorted in a Z-order. A widget is clipped by its parent and by the
    # widgets in front of it. http://pyqt.sourceforge.net/Docs/PyQt4/qwidget.html
    userInterfaceWidget = QWidget()

    # Set window size.
    userInterfaceWidget.resize( 1120, 640 )

    # Set window title
    userInterfaceWidget.setWindowTitle( "Drunk Walk Simulator" )

    # https://github.com/GNOME/adwaita-icon-theme
    # https://code.google.com/archive/p/faenza-icon-theme/
    mainApplicationIcon = QIcon( 'login.png' )

    # PyQt4 set windows taskbar icon
    # https://stackoverflow.com/questions/12432637/pyqt4-set-windows-taskbar-icon
    # https://stackoverflow.com/questions/44161669/how-to-set-a-python-qt4-window-icon
    userInterfaceWidget.setWindowIcon( mainApplicationIcon )

    # Show window
    userInterfaceWidget.show()

    # The mainloop of the application. The event handling starts from this point. The mainloop
    # receives events from the window system and dispatches them to the application widgets. The
    # mainloop ends if we call the exit() method or the main widget is destroyed. The sys.exit()
    # method ensures a clean exit. The environment will be informed how the application ended.
    #
    # The exec_() method has an underscore. It is because the exec is a Python keyword. And thus,
    # exec_() was used instead. http://zetcode.com/gui/pyqt4/firstprograms/
    sys.exit( simulatorApplication.exec_() )



if __name__ == '__main__':
    main()


