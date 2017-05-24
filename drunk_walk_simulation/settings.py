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

import debug_tools


# Enable debug messages: (bitwise)
#
# 0   - Disabled debugging.
# 1   - Basic logging messages.
# 2   - ...
#
# 127 - All debugging levels at the same time.
debug_tools.g_debug_level = 127


# How many stickies each player has when the game begins.
INITIAL_HAND_STICKIES = 3


# How many AI players are going to join when the game begins.
INITIAL_AI_PLAYERS = 5


# Bet tree location
BET_TREE_LOCATION = "D:/User/Downloads/tree.txt"


class TYPES():
    AI     = 0
    RANDOM = 1


# Whether to use the AI or range valid random numbers to play the game.
GAME_FLOW_TYPE = TYPES.RANDOM





