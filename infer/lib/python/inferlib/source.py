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

from . import colorize, config

BASE_INDENT = 2
# how many lines of context around each report
SOURCE_CONTEXT = 2


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
        return self

    def indent_pop(self):
        return self.indent.pop()
        return self

    def newline(self):
        self.text += '\n'
        return self

    def add(self, x):
        if type(x) != unicode:
            x = x.decode(config.CODESET)
        lines = x.splitlines()
        indent = self.indent_get()
        lines = [indent + l for l in lines]
        self.text += '\n'.join(lines)
        return self

    def __unicode__(self):
        return self.text

    def __str__(self):
        return unicode(self).encode(config.CODESET)


def build_source_context(source_name, mode, report_line):
    start_line = max(1, report_line - SOURCE_CONTEXT)
    # could go beyond last line, checked in the loop
    end_line = report_line + SOURCE_CONTEXT

    # get source excerpt
    line_number = 1
    excerpt = ''
    with codecs.open(source_name, 'r',
                     encoding=config.CODESET, errors="replace") as source_file:
        # avoid going past the end of the file
        for line in source_file:
            if start_line <= line_number <= end_line:
                excerpt += line
            line_number += 1
    excerpt = colorize.syntax_highlighting(source_name, mode, excerpt)

    # number lines and add caret at the right position
    n_length = len(str(end_line))
    s = ''
    line_number = start_line
    for line in excerpt.split('\n'):
        num = colorize.color((str(line_number) + '.').zfill(n_length),
                             colorize.DIM, mode)
        caret = '  '
        if line_number == report_line:
            caret = colorize.color('> ',
                                   colorize.HEADER, mode)
        s += '%s %s%s\n' % (num, caret, line)
        line_number += 1

    return s
