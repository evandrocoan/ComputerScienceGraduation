# -*- coding: UTF-8 -*-
#
# This first line allow to use UTF-8 encoding on this file.
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

import debug_tools
from debug_tools import log


# Enable debug messages: (bitwise)
#
# 0   - Disabled debugging
# 1   - Basic logging messages
# 2   - Simulator debugging
# 4   - Drawing panel debugging
#
# 127 - All debugging levels at the same time.
debug_tools.g_debug_level = 127
log( 1, "Importing " + __name__ )


#
MINIMUM_STEPS_TO_SHOW_PARTIAL_PROGRESS = 99999







