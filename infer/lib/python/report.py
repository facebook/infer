#!/usr/bin/env python2.7

# Copyright (c) 2016 - present Facebook, Inc.
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
import os
import sys

from inferlib import config, issues, utils

arg_parser = argparse.ArgumentParser(add_help=False)
arg_parser.add_argument('--issues-csv', metavar='<file>',
                        help='Location of the csv report (ignored for now)')
arg_parser.add_argument('--issues-json', metavar='<file>', required=True,
                        help='Location of the json report')
arg_parser.add_argument('--issues-txt', metavar='<file>',
                        help='Location of the text report (ignored for now)')
arg_parser.add_argument('--issues-xml', metavar='<file>',
                        help='Location of the xml report (ignored for now)')
arg_parser.add_argument('--pmd-xml', action='store_true',
                        help='Output issues in (PMD) XML format.')
arg_parser.add_argument('--project-root', metavar='<directory>', required=True,
                        help='Location of the project root')
arg_parser.add_argument('--results-dir', metavar='<directory>', required=True,
                        help='Location of the results directory')


def main():
    sys_argv = map(utils.decode, sys.argv)
    args = arg_parser.parse_args(sys_argv[1:])
    bugs_out = os.path.join(args.results_dir, config.BUGS_FILENAME)
    issues.print_and_save_errors(args.results_dir, args.project_root,
                                 args.issues_json, bugs_out, args.pmd_xml)


if __name__ == '__main__':
    main()
