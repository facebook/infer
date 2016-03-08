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


def syntax_highlighting(source_name, mode, s):
    if pygments is None or mode == PLAIN_FORMATTER:
        return s

    lexer = pygments.lexers.get_lexer_for_filename(source_name)
    formatter = None
    if mode == TERMINAL_FORMATTER:
        if not sys.stdout.isatty():
            return s
        formatter = pygments.formatters.TerminalFormatter()
    return pygments.highlight(s, lexer, formatter)
