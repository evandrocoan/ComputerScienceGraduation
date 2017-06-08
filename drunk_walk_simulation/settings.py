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
# 8   - Main window debugging
#
# 127 - All debugging levels at the same time.
debug_tools.g_debug_level = 127
log( 1, "Importing " + __name__ )


# Configure how responsive the progress bar is.
COMPENSATION_FOR_TOTAL_CYCLES = 100
MINIMUM_STEPS_WHEN_DRAWING_THE_PATH    = 999
MINIMUM_STEPS_TO_SHOW_PARTIAL_PROGRESS = 99999


# When there are more lines than this, draw simple lines for performance.
MAXIMUM_FULL_LINE_SIZE = 99999


# To plot the path lengths we need to create an array which cannot pass this limit, otherwise
# a memory overflow will be throw, 9.999.999
MAXIMUM_COMPUTABLE_SIZE = 9999999




def increaseAxe( increasePoint, oppositePoint, extraScreen=200 ):

    if increasePoint < 0:

        if oppositePoint >= 0:
            return increasePoint - extraScreen

        else:
            return increasePoint + extraScreen

    if oppositePoint < 0:
        return increasePoint + extraScreen

    return increasePoint - extraScreen



def intWithCommas(x):
    """
        How to print number with commas as thousands separators?
        https://stackoverflow.com/questions/1823058/how-to-print-number-with-commas-as-thousands-separators
    """

    if type(x) not in [type(0), type(0L)]:
        raise TypeError("Parameter must be an integer.")

    if x < 0:
        return '-' + intWithCommas(-x)

    result = ''

    while x >= 1000:
        x, r = divmod(x, 1000)
        result = ".%03d%s" % (r, result)

    return "%d%s" % (x, result)

