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
import json
import logging
import multiprocessing
import os
import shutil
import subprocess
import sys
import time

from . import config, issues, utils

# Increase the limit of the CSV parser to sys.maxlimit
csv.field_size_limit(sys.maxsize)

INFER_ANALYZE_BINARY = 'InferAnalyze'


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
base_group.add_argument('-c', '--continue', action='store_true',
                        dest='continue_capture',
                        help='''Continue the capture for the reactive
                        analysis, increasing the changed files/procedures.''')
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

infer_group.add_argument('--buck', action='store_true', dest='buck',
                         help='To use when run with buck')

infer_group.add_argument('--java-jar-compiler',
                         metavar='<file>')


def remove_infer_out(infer_out):
    # it is safe to ignore errors here because recreating the infer_out
    # directory will fail later
    shutil.rmtree(infer_out, True)


def create_results_dir(results_dir):
    utils.mkdir_if_not_exists(results_dir)
    utils.mkdir_if_not_exists(os.path.join(results_dir, 'specs'))
    utils.mkdir_if_not_exists(os.path.join(results_dir, 'captured'))
    utils.mkdir_if_not_exists(os.path.join(results_dir, 'sources'))


def reset_start_file(results_dir, touch_if_present=False):
    start_path = os.path.join(results_dir, '.start')
    if (not os.path.exists(start_path)) or touch_if_present:
        # create new empty file - this will update modified timestamp
        open(start_path, 'w').close()


def clean(infer_out):
    directories = [
        'multicore', 'classnames', 'sources',
        config.JAVAC_FILELISTS_FILENAME,
    ]
    extensions = ['.cfg', '.cg']

    for root, dirs, files in os.walk(infer_out):
        for d in dirs:
            if d in directories:
                path = os.path.join(root, d)
                shutil.rmtree(path)
        for f in files:
            for ext in extensions:
                if f.endswith(ext):
                    path = os.path.join(root, f)
                    os.remove(path)


def help_exit(message):
    utils.stdout(message)
    infer_parser.print_usage()
    exit(1)


def run_command(cmd, debug_mode, javac_arguments, step, analyzer):
    if debug_mode:
        utils.stdout('\n{0}\n'.format(' '.join(cmd)))
    try:
        return subprocess.check_call(cmd)
    except subprocess.CalledProcessError as e:
        error_msg = 'Failure during {0}, original command was\n\n{1}\n\n'
        infer_cmd = ['infer', '-g', '-a', analyzer]
        failing_cmd = infer_cmd + ['--', 'javac'] + javac_arguments
        logging.error(error_msg.format(
            step,
            failing_cmd
        ))
        raise e


class AnalyzerWrapper(object):

    javac = None

    def __init__(self, args):
        self.args = args
        if self.args.analyzer not in config.ANALYZERS:
            help_exit('Unknown analysis mode \"{0}\"'
                      .format(self.args.analyzer))

        if self.args.infer_out is None:
            help_exit('Expect Infer results directory')
        try:
            os.mkdir(self.args.infer_out)
        except OSError as e:
            if not os.path.isdir(self.args.infer_out):
                raise e

    def clean_exit(self):
        if os.path.isdir(self.args.infer_out):
            utils.stdout('removing {}'.format(self.args.infer_out))
            shutil.rmtree(self.args.infer_out)
        exit(os.EX_OK)

    def analyze(self):
        logging.info('Starting analysis')
        infer_analyze = [
            utils.get_cmd_in_bin_dir(INFER_ANALYZE_BINARY),
            '-results_dir',
            self.args.infer_out
        ]
        exit_status = os.EX_OK

        javac_original_arguments = \
            self.javac.original_arguments if self.javac is not None else []

        if self.args.multicore == 1:
            analyze_cmd = infer_analyze
            exit_status = run_command(
                analyze_cmd,
                self.args.debug,
                javac_original_arguments,
                'analysis',
                self.args.analyzer
            )
        else:
            multicore_dir = os.path.join(self.args.infer_out, 'multicore')
            pwd = os.getcwd()
            if os.path.isdir(multicore_dir):
                shutil.rmtree(multicore_dir)
            os.mkdir(multicore_dir)
            os.chdir(multicore_dir)
            analyze_cmd = infer_analyze + ['-makefile', 'Makefile']
            makefile_status = run_command(
                analyze_cmd,
                self.args.debug,
                javac_original_arguments,
                'create_makefile',
                self.args.analyzer
            )
            exit_status += makefile_status
            if makefile_status == os.EX_OK:
                make_cmd = ['make', '-k']
                make_cmd += ['-j', str(self.args.multicore)]
                if self.args.load_average is not None:
                    make_cmd += ['-l', str(self.args.load_average)]
                if not self.args.debug:
                    make_cmd += ['-s']
                make_status = run_command(
                    make_cmd,
                    self.args.debug,
                    javac_original_arguments,
                    'run_makefile',
                    self.args.analyzer
                )
                os.chdir(pwd)
                exit_status += make_status

        if self.args.buck and exit_status == os.EX_OK:
            clean(self.args.infer_out)

        return exit_status

    def create_report(self):
        """Report statistics about the computation and create a CSV file
        containing the list or errors found during the analysis"""

        out_dir = self.args.infer_out
        json_report = os.path.join(out_dir, config.JSON_REPORT_FILENAME)
        procs_report = os.path.join(self.args.infer_out, 'procs.csv')

        infer_print_cmd = [utils.get_cmd_in_bin_dir('InferPrint')]
        infer_print_options = [
            '-q',
            '-results_dir', self.args.infer_out,
            '-bugs_json', json_report,
            '-procs', procs_report,
            '-analyzer', self.args.analyzer
        ]
        if self.args.debug or self.args.debug_exceptions:
            infer_print_options.append('-with_infer_src_loc')
        exit_status = subprocess.check_call(
            infer_print_cmd + infer_print_options
        )
        if exit_status != os.EX_OK:
            logging.error(
                'Error with InferPrint with the command: {}'.format(
                    infer_print_cmd))

        return exit_status

    def report(self):
        report_status = self.create_report()
        if report_status == os.EX_OK and not self.args.buck:
            infer_out = self.args.infer_out
            json_report = os.path.join(infer_out, config.JSON_REPORT_FILENAME)
            bugs_out = os.path.join(infer_out, config.BUGS_FILENAME)
            issues.print_and_save_errors(infer_out, self.args.project_root,
                                         json_report, bugs_out,
                                         self.args.pmd_xml)

    def analyze_and_report(self):
        if self.args.analyzer not in [config.ANALYZER_COMPILE,
                                      config.ANALYZER_CAPTURE]:
            if self.args.analyzer == config.ANALYZER_LINTERS:
                self.report()
            elif self.analyze() == os.EX_OK:
                self.report()
