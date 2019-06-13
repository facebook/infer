# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import codecs
import os

from . import colorize, config, utils

BASE_INDENT = 2
# how many lines of context around each report
SOURCE_CONTEXT = 2


class Indenter(unicode):
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
            x = utils.decode(x)
        lines = x.splitlines()
        indent = self.indent_get()
        lines = [indent + l for l in lines]
        self.text += '\n'.join(lines)
        return self

    def __unicode__(self):
        return self.text

    def __str__(self):
        return utils.encode(unicode(self))


def build_source_context(source_name, mode, report_line,
                         report_col, empty_desc):
    start_line = max(1, report_line - SOURCE_CONTEXT)
    start_col = max(0, report_col)
    # could go beyond last line, checked in the loop
    end_line = report_line + SOURCE_CONTEXT

    # get source excerpt
    line_number = 1
    excerpt = ''
    if not os.path.isfile(source_name):
        return ''
    with codecs.open(source_name, 'r',
                     encoding=config.CODESET, errors="replace") as source_file:
        # avoid going past the end of the file
        for line in source_file:
            last_line = line_number
            if start_line <= line_number <= end_line:
                excerpt += line
            elif line_number > end_line:
                # OPTIM: no need to read past the last line of the excerpt
                break
            line_number += 1
    excerpt = colorize.syntax_highlighting(source_name, mode, excerpt)

    # number lines and add caret at the right position
    n_length = len(str(last_line))
    s = ''
    line_number = start_line
    for line in excerpt.split('\n')[:-1]:
        num = colorize.color((str(line_number) + '.').zfill(n_length),
                             colorize.DIM, mode)
        caret = '  '
        do_mark_column = (line_number == report_line and
                          start_col > 1 and not empty_desc)

        # mark the line if we are not also marking the column
        if line_number == report_line and not do_mark_column:
            caret = colorize.color('> ',
                                   colorize.BLUE + colorize.BRIGHT, mode)
        s += '%s %s%s\n' % (num, caret, line)
        # mark the column position
        if do_mark_column:
            pad = ' ' * (3 + n_length + start_col)
            s += pad + colorize.color('^',
                                      colorize.BLUE + colorize.BRIGHT,
                                      mode) + '\n'
        line_number += 1

    return s
