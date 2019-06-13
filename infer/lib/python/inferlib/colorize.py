# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

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
GREEN = terminal_only('\033[32m')
RED = terminal_only('\033[31m')
RESET = terminal_only('\033[0m')
WHITE = terminal_only('\033[37m')
WHITE_BG = terminal_only('\033[47m')
YELLOW = terminal_only('\033[33m')

HEADER = BRIGHT
SUCCESS = BLUE_BG + WHITE + BRIGHT

ERROR = RED
WARNING = YELLOW
ADVICE = BLUE
LIKE = GREEN


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
    # there's a bug in pygments.highlight() where it will remove all starting
    # newline characters, so we have to add them back!
    initial_newlines = ''
    i = 0
    while (i < len(s) and s[i] == '\n'):
        initial_newlines += '\n'
        i += 1
    # pygments.highlight() also insists that all string end with exactly one
    # newline character regardless of the input string!
    final_newlines = ''
    i = 1
    while (i <= len(s) and s[-i] == '\n'):
        final_newlines += '\n'
        i += 1
    colorized_string = pygments.highlight(s, lexer, formatter)
    # strip the result from pygments.highlight() to get rid of the
    # potentially spurious final newline, and also to continue to
    # work in case the bugs in pygments.highlight() gets fixed.
    return initial_newlines + colorized_string.strip('\n') + final_newlines


def color(s, color, mode):
    if mode == TERMINAL_FORMATTER:
        return color + s + RESET
    if mode == PLAIN_FORMATTER:
        return s
    raise Invalid_mode()
