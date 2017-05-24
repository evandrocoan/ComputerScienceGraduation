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
from main_window import MainWindow


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

    ex = MainWindow()

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


