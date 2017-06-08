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

import argparse
import csv
import multiprocessing
import sys


from . import config, utils

# Increase the limit of the CSV parser to sys.maxlimit
csv.field_size_limit(sys.maxsize)


base_parser = argparse.ArgumentParser(add_help=False)
base_group = base_parser.add_argument_group('global arguments')
base_group.add_argument('-o', '--out', metavar='<directory>',
                        default=utils.encode(config.DEFAULT_INFER_OUT),
                        dest='infer_out',
                        type=utils.decode,
                        action=utils.AbsolutePathAction,
                        help='Set the Infer results directory')
base_group.add_argument('-r', '--reactive', action='store_true',
                        help='''Analyze in reactive propagation mode
                        starting from changed files.''')
base_group.add_argument('--debug-exceptions', action='store_true',
                        help='''Generate lightweight debugging information:
                        just print the internal exceptions during analysis''')
base_group.add_argument('-g', '--debug', action='store_true',
                        help='Generate all debugging information')
base_group.add_argument('-a', '--analyzer',
                        help='Select the analyzer within: {0}'.format(
                            ', '.join(config.ANALYZERS)),
                        default=config.ANALYZER_INFER)
base_group.add_argument('-nf', '--no-filtering', action='store_true',
                        help='''Also show the results from the experimental
                        checks. Warning: some checks may contain many false
                        alarms''')

base_group.add_argument('--android-harness', action='store_true',
                        help='''[experimental] Create harness to detect bugs
                        involving the Android lifecycle''')

base_group.add_argument('--pmd-xml',
                        action='store_true',
                        help='''Output issues in (PMD) XML format.''')


infer_parser = argparse.ArgumentParser(parents=[base_parser])
infer_group = infer_parser.add_argument_group('backend arguments')
infer_group.add_argument('-pr', '--project-root',
                         dest='project_root',
                         help='Location of the project root '
                         '(default is current directory)')
infer_group.add_argument('-j', '--multicore', metavar='n', type=int,
                         default=multiprocessing.cpu_count(),
                         dest='multicore', help='Set the number of cores to '
                         'be used for the analysis (default uses all cores)')
infer_group.add_argument('-l', '--load-average', metavar='<float>', type=float,
                         help='Specifies that no new jobs (commands) should '
                         'be started if there are others jobs running and the '
                         'load average is at least <float>.')

infer_group.add_argument('--java-jar-compiler',
                         metavar='<file>')
