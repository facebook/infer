#!/usr/bin/env python2.7

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import argparse
import sys

from inferlib import issues, utils

arg_parser = argparse.ArgumentParser(add_help=False)
arg_parser.add_argument('--issues-json', metavar='<file>', required=True,
                        help='Location of the json report')
arg_parser.add_argument('--issues-txt', metavar='<file>',
                        help='Location of the text report')
arg_parser.add_argument('--issues-xml', metavar='<file>',
                        help='Location of the xml report (ignored for now)')
arg_parser.add_argument('--pmd-xml', action='store_true',
                        help='Output issues in (PMD) XML format.')
arg_parser.add_argument('--project-root', metavar='<directory>', required=True,
                        help='Location of the project root')
arg_parser.add_argument('--quiet', action='store_true',
                        help='Silence console output.')
arg_parser.add_argument('--results-dir', metavar='<directory>', required=True,
                        help='Location of the results directory')


def main():
    sys_argv = map(utils.decode, sys.argv)
    args = arg_parser.parse_args(sys_argv[1:])
    issues.print_and_save_errors(args.results_dir, args.project_root,
                                 args.issues_json, args.issues_txt,
                                 args.pmd_xml, console_out=not args.quiet)


if __name__ == '__main__':
    main()
