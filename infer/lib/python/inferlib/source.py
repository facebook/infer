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

import codecs
try:
    import pygments
    import pygments.formatters
    import pygments.lexers
except ImportError:
    pygments = None
import sys

from . import config, utils

BASE_INDENT = 2
# how many lines of context around each report
SOURCE_CONTEXT = 2

# syntax highlighting modes
PLAIN_FORMATTER = 0
TERMINAL_FORMATTER = 1


class Indenter(str):
    def __init__(self):
        super(Indenter, self).__init__()
        self.text = ''
        self.indent = []

    def indent_get(self):
        indent = ''
        for i in self.indent:
            indent += i
        return indent

    def indent_push(self, n=1):
        self.indent.append(n * BASE_INDENT * ' ')

    def indent_pop(self):
        return self.indent.pop()

    def newline(self):
        self.text += '\n'

    def add(self, x):
        if type(x) != unicode:
            x = x.decode(config.LOCALE)
        lines = x.splitlines()
        indent = self.indent_get()
        lines = [indent + l for l in lines]
        self.text += '\n'.join(lines)

    def __unicode__(self):
        return self.text

    def __str__(self):
        return unicode(self).encode(config.LOCALE)


def build_source_context(source_name, mode, report_line):
    start_line = max(1, report_line - SOURCE_CONTEXT)
    # could go beyond last line, checked in the loop
    end_line = report_line + SOURCE_CONTEXT

    n_length = len(str(end_line))
    line_number = 1
    s = ''
    with codecs.open(source_name, 'r', encoding=config.LOCALE) as source_file:
        for line in source_file:
            if start_line <= line_number <= end_line:
                num = str(line_number).zfill(n_length)
                caret = '  '
                if line_number == report_line:
                    caret = '> '
                s += u'%s. %s%s' % (num, caret, line)
            line_number += 1
    return _syntax_highlighting(source_name, mode, s)


def _syntax_highlighting(source_name, mode, s):
    if pygments is None or mode == PLAIN_FORMATTER:
        return s

    lexer = pygments.lexers.get_lexer_for_filename(source_name)
    formatter = None
    if mode == TERMINAL_FORMATTER:
        if not sys.stdout.isatty():
            return s
        formatter = pygments.formatters.TerminalFormatter()
    return pygments.highlight(s, lexer, formatter)
