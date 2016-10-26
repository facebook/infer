# Copyright (c) 2013 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import sys

try:
    import pygments
    import pygments.formatters
    import pygments.lexers
except ImportError:
    pygments = None

# syntax highlighting modes
PLAIN_FORMATTER = 0
TERMINAL_FORMATTER = 1


def terminal_only(s):
    if not sys.stdout.isatty():
        return ''
    return s

BLUE = terminal_only('\033[34m')
BLUE_BG = terminal_only('\033[44m')
MAGENTA = terminal_only('\033[35m')
MAGENTA_BG = terminal_only('\033[45m')
BRIGHT = terminal_only('\033[1m')
DIM = terminal_only('\033[2m')
RED = terminal_only('\033[31m')
RESET = terminal_only('\033[0m')
WHITE = terminal_only('\033[37m')
WHITE_BG = terminal_only('\033[47m')
YELLOW = terminal_only('\033[35m')

ERROR = RED
HEADER = BRIGHT
SUCCESS = BLUE_BG + WHITE + BRIGHT
WARNING = ''
ADVICE = ''


class Invalid_mode(Exception):
    pass


def syntax_highlighting(source_name, mode, s):
    if pygments is None or mode == PLAIN_FORMATTER:
        return s

    try:
        lexer = pygments.lexers.get_lexer_for_filename(source_name)
    except pygments.lexers.ClassNotFound:
        return s

    formatter = None
    if mode == TERMINAL_FORMATTER:
        if not sys.stdout.isatty():
            return s
        formatter = pygments.formatters.TerminalFormatter()
    return pygments.highlight(s, lexer, formatter)


def color(s, color, mode):
    if mode == TERMINAL_FORMATTER:
        return color + s + RESET
    if mode == PLAIN_FORMATTER:
        return s
    raise Invalid_mode()
