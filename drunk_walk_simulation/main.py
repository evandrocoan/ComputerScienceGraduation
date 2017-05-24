#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
import sys
from PyQt4.QtGui import *

# Create an PyQT4 application object.
a = QApplication(sys.argv)

# The QWidget widget is the base class of all user interface objects in PyQt4.
w = QWidget()

# Set window size.
w.resize(820, 240)

# Set window title
w.setWindowTitle("Hello World!")

# https://github.com/GNOME/adwaita-icon-theme
# https://code.google.com/archive/p/faenza-icon-theme/
undoicon = QIcon('login.png')

# PyQt4 set windows taskbar icon
# https://stackoverflow.com/questions/12432637/pyqt4-set-windows-taskbar-icon
# https://stackoverflow.com/questions/44161669/how-to-set-a-python-qt4-window-icon
w.setWindowIcon(undoicon)

# Show window
w.show()

sys.exit(a.exec_())













