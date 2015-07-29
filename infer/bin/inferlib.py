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
import glob
import json
import logging
import multiprocessing
import os
import shutil
import subprocess
import sys
import tempfile
import time
import xml.etree.ElementTree as ET

# Increase the limit of the CSV parser to sys.maxlimit
csv.field_size_limit(sys.maxsize)

# Infer imports
import jwlib
import utils

# list of analysis options
INFER = 'infer'
ERADICATE = 'eradicate'
CHECKERS = 'checkers'
CAPTURE = 'capture'
COMPILE = 'compile'
TRACING = 'tracing'

MODES = [COMPILE, CAPTURE, INFER, ERADICATE, CHECKERS, TRACING]

INFER_ANALYZE_BINARY = "InferAnalyze"

ERROR = 'ERROR'
WARNING = 'WARNING'
INFO = 'INFO'


def get_infer_version():
    try:
        return subprocess.check_output([
            utils.get_cmd_in_bin_dir(INFER_ANALYZE_BINARY), '-version'])
    except:
        print("Failed to run {0} binary, exiting".
              format(INFER_ANALYZE_BINARY))
        sys.exit(os.EX_UNAVAILABLE)


# https://github.com/python/cpython/blob/aa8ea3a6be22c92e774df90c6a6ee697915ca8ec/Lib/argparse.py
class VersionAction(argparse._VersionAction):
    def __call__(self, parser, namespace, values, option_string=None):
        # set self.version so that argparse version action knows it
        self.version = get_infer_version()
        super(VersionAction, self).__call__(parser,
                                            namespace,
                                            values,
                                            option_string)




base_parser = argparse.ArgumentParser(add_help=False)
base_group = base_parser.add_argument_group('global arguments')
base_group.add_argument('-o', '--out', metavar='<directory>',
                        default=utils.DEFAULT_INFER_OUT, dest='infer_out',
                        action=utils.AbsolutePathAction,
                        help='Set the Infer results directory')
base_group.add_argument('-i', '--incremental', action='store_true',
                        help='''Do not delete the results directory across
                        Infer runs''')
base_group.add_argument('-g', '--debug', action='store_true',
                        help='Generate extra debugging information')
base_group.add_argument('-a', '--analyzer',
                        help='Select the analyzer within: {0}'.format(
                            ', '.join(MODES)),
                        default=INFER)
base_group.add_argument('-nf', '--no-filtering', action='store_true',
                        help='''Also show the results from the experimental
                        checks. Warning: some checks may contain many false
                        alarms''')

base_group.add_argument('--log_to_stderr', action='store_true',
                        help='''When set, all logging will go to stderr instead
                        of log file''')
base_parser.add_argument('-v', '--version', help='Get version of the analyzer',
                         action=VersionAction)


inferJ_parser = argparse.ArgumentParser(parents=[base_parser])
inferJ_group = inferJ_parser.add_argument_group('backend arguments')
inferJ_group.add_argument('-j', '--multicore', metavar='n', type=int,
                           default=multiprocessing.cpu_count(),
                           dest='multicore', help='Set the number of cores to '
                           'be used for the analysis (default uses all cores)')
inferJ_group.add_argument('-x', '--project', metavar='<projectname>',
                           help='Project name, for recording purposes only')

inferJ_group.add_argument('-r', '--revision', metavar='<githash>',
                           help='The githash, for recording purposes only')

inferJ_group.add_argument('--buck', action='store_true', dest='buck',
                           help='To use when run with buck')

inferJ_group.add_argument('--infer_cache', metavar='<directory>',
                           help='Select a directory to contain the infer cache')

inferJ_group.add_argument('-pr', '--project_root',
                           dest='project_root',
                           default=os.getcwd(),
                           help='Location of the project root '
                                '(default is current directory)')

inferJ_group.add_argument('--objc_ml_buckets',
                           dest='objc_ml_buckets',
                           help='memory leak buckets to be checked, '
                                'separated by commas. The possible '
                                'buckets are cf (Core Foundation), '
                                'arc, narc (No arc)')

inferJ_group.add_argument('-nt', '--notest', action='store_true',
                           dest='notest',
                           help='Prints output of symbolic execution')

inferJ_group.add_argument('--specs-dir',
                          metavar='<dir>',
                          action='append',
                          dest='specs_dirs',
                          help='add dir to the list of directories to be '
                               'searched for spec files. Repeat the argument '
                               'in case multiple folders are needed')

def detect_javac(args):
    for index, arg in enumerate(args):
        if arg == 'javac':
            return index


def get_inferJ_args(args):
    index = detect_javac(args)
    if index is None:
        cmd_args = args
    else:
        cmd_args = args[:index]
    return inferJ_parser.parse_args(cmd_args)


def get_javac_args(args):
    javac_args = args[detect_javac(args) + 1:]
    if len(javac_args) == 0:
        return None
    else:
        # replace any -g:.* flag with -g to preserve debugging symbols
        args = map(lambda arg: '-g' if '-g:' in arg else arg, javac_args)
        # skip -Werror
        args = filter(lambda arg: arg != '-Werror', args)
        return args


def remove_infer_out(infer_out):
    # it is safe to ignore errors here because recreating the infer_out
    # directory will fail later
    shutil.rmtree(infer_out, True)


def mkdir_if_not_exists(path):
    try:
        os.mkdir(path)
    except OSError:
        pass

def create_results_dir(results_dir):
    mkdir_if_not_exists(results_dir)
    mkdir_if_not_exists(os.path.join(results_dir, 'specs'))
    mkdir_if_not_exists(os.path.join(results_dir, 'captured'))
    mkdir_if_not_exists(os.path.join(results_dir, 'sources'))


def clean_infer_out(infer_out):

    directories = ['multicore', 'classnames', 'sources', 'filelists']
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
    print(message)
    inferJ_parser.print_usage()
    exit(1)


def compare_rows(row_1, row_2):
    filename_1 = row_1[utils.CSV_INDEX_FILENAME]
    filename_2 = row_2[utils.CSV_INDEX_FILENAME]
    if filename_1 < filename_2:
        return -1
    elif filename_1 > filename_2:
        return 1
    else:
        line_1 = int(row_1[utils.CSV_INDEX_LINE])
        line_2 = int(row_2[utils.CSV_INDEX_LINE])
        return line_1 - line_2


def should_report(analyzer, row):
    error_kind = row[utils.CSV_INDEX_KIND]
    error_type = row[utils.CSV_INDEX_TYPE]
    error_bucket = ''  # can be updated later once we extract it from qualifier

    try:
        qualifier_xml = ET.fromstring(row[utils.CSV_INDEX_QUALIFIER_TAGS])
        if qualifier_xml.tag == utils.QUALIFIER_TAGS:
            bucket = qualifier_xml.find(utils.BUCKET_TAGS)
            if bucket is not None:
                error_bucket = bucket.text
    except ET.ParseError:
        pass  # this will skip any invalid xmls

    # config what to print is listed below
    error_kinds = [ERROR, WARNING]

    null_style_bugs = [
        'NULL_DEREFERENCE',
        'PARAMETER_NOT_NULL_CHECKED',
        'IVAR_NOT_NULL_CHECKED',
        'PREMATURE_NIL_TERMINATION_ARGUMENT',
    ]
    null_style_buckets = ['B1', 'B2']

    other_bugs = [
        'RESOURCE_LEAK',
        'MEMORY_LEAK',
        'RETAIN_CYCLE',
        'ASSERTION_FAILURE'
    ]

    if analyzer in [ERADICATE, CHECKERS, TRACING]:
        # report all issues for eradicate and checkers
        return True

    if error_kind not in error_kinds:
        return False

    if not error_type:
        return False

    if error_type in null_style_bugs:
        if error_bucket and error_bucket in null_style_buckets:
            return True
        else:
            return False
    elif error_type in other_bugs:
        return True
    else:
        return False


def clean_csv(args, csv_report):
    collected_rows = []
    with open(csv_report, 'r') as file_in:
        reader = csv.reader(file_in)
        rows = [row for row in reader]
        if len(rows) <= 1:
            return rows
        else:
            for row in rows[1:]:
                filename = row[utils.CSV_INDEX_FILENAME]
                if os.path.isfile(filename):
                    if args.no_filtering or should_report(args.analyzer, row):
                        collected_rows.append(row)
            collected_rows = sorted(
                collected_rows,
                cmp=compare_rows)
            collected_rows = [rows[0]] + collected_rows
    temporary_file = tempfile.mktemp()
    with open(temporary_file, 'w') as file_out:
        writer = csv.writer(file_out)
        writer.writerows(collected_rows)
        file_out.flush()
        shutil.move(temporary_file, csv_report)


def print_and_write(file_out, message):
    print(message)
    file_out.write(message + '\n')


def print_errors(csv_report, bugs_out):
    with open(csv_report, 'r') as file_in:
        reader = csv.reader(file_in)
        reader.next()  # first line is header, skip it

        errors = filter(
            lambda row: row[utils.CSV_INDEX_KIND] in [ERROR, WARNING],
            reader
        )

        with open(bugs_out, 'w') as file_out:
            if not errors:
                print_and_write(file_out, 'No issues found')
            for row in errors:
                filename = row[utils.CSV_INDEX_FILENAME]
                if os.path.isfile(filename):
                    kind = row[utils.CSV_INDEX_KIND]
                    line = row[utils.CSV_INDEX_LINE]
                    error_type = row[utils.CSV_INDEX_TYPE]
                    msg = row[utils.CSV_INDEX_QUALIFIER]
                    print_and_write(
                        file_out,
                        '{0}:{1}: {2}: {3}\n  {4}\n'.format(
                            filename,
                            line,
                            kind.lower(),
                            error_type,
                            msg,
                        )
                    )


def run_command(cmd, debug_mode, javac_arguments, step, analyzer):
    if debug_mode:
        print('\n{0}\n'.format(' '.join(cmd)))
    try:
        return subprocess.check_call(cmd)
    except subprocess.CalledProcessError as e:
        error_msg = 'Failure during {0}, original command was\n\n{1}\n\n'
        inferJ_cmd = ['inferJ', '-g', '-a', analyzer]
        failing_cmd = inferJ_cmd + ['javac'] + javac_arguments
        logging.error(error_msg.format(
            step,
            failing_cmd
        ))
        raise e


class Infer:

    def __init__(self, args, javac_args):

        self.args = args
        if self.args.analyzer not in MODES:
            help_exit(
                'Unknown analysis mode \"{0}\"'.format(self.args.analyzer)
            )

        utils.configure_logging(self.args.debug)

        self.javac = jwlib.CompilerCall(javac_args)

        if not self.javac.args.version:
            if javac_args is None:
                help_exit('No javac command detected')

            if self.args.infer_out is None:
                help_exit('Expect Infer results directory')

            if self.args.buck:
                self.args.infer_out = os.path.join(
                    self.javac.args.classes_out,
                    utils.BUCK_INFER_OUT)
                self.args.infer_out = os.path.abspath(self.args.infer_out)

            try:
                os.mkdir(self.args.infer_out)
            except OSError as e:
                if not os.path.isdir(self.args.infer_out):
                    raise e

            self.stats = {'int': {}, 'float': {}}
            self.timing = {}

        if self.args.specs_dirs:
            # Each dir passed in input is prepended by '-lib'.
            # Convert each path to absolute because when running from
            # cluster Makefiles (multicore mode) InferAnalyze creates the wrong
            # absolute path from within the multicore folder
            self.args.specs_dirs = [item
                                    for argument in
                                    (['-lib', os.path.abspath(path)] for path in
                                     self.args.specs_dirs)
                                    for item in argument]


    def clean_exit(self):
        if os.path.isdir(self.args.infer_out):
            print('removing', self.args.infer_out)
            shutil.rmtree(self.args.infer_out)
        exit(os.EX_OK)

    def run_infer_frontend(self):

        infer_cmd = [utils.get_cmd_in_bin_dir('InferJava')]

        if self.args.buck:
            infer_cmd += ['-project_root', os.getcwd()]

        infer_cmd += [
            '-results_dir', self.args.infer_out,
            '-verbose_out', self.javac.verbose_out,
        ]

        if os.path.isfile(utils.MODELS_JAR):
            infer_cmd += ['-models', utils.MODELS_JAR]

        infer_cmd.append('-no-static_final')

        if self.args.debug:
            infer_cmd.append('-debug')
        if self.args.analyzer == TRACING:
            infer_cmd.append('-tracing')

        return run_command(
            infer_cmd,
            self.args.debug,
            self.javac.original_arguments,
            'frontend',
            self.args.analyzer
        )

    def compile(self):
        return self.javac.run()

    def capture(self):
        javac_status = self.compile()
        if javac_status == os.EX_OK:
            return self.run_infer_frontend()
        else:
            return javac_status

    def analyze(self):
        logging.info('Starting analysis')
        infer_analyze = [
            utils.get_cmd_in_bin_dir(INFER_ANALYZE_BINARY),
            '-results_dir',
            self.args.infer_out
        ]
        infer_options = []

        # remove specs if possible so that old issues are less likely
        # to be reported
        infer_options += ['-allow_specs_cleanup']

        if self.args.analyzer == ERADICATE:
            infer_options += ['-checkers', '-eradicate']
        elif self.args.analyzer == CHECKERS:
            infer_options += ['-checkers']
        else:
            if self.args.analyzer == TRACING:
                infer_options.append('-tracing')
            if os.path.isfile(utils.MODELS_JAR):
                infer_options += ['-models', utils.MODELS_JAR]

        if self.args.infer_cache:
            infer_options += ['-infer_cache', self.args.infer_cache]

        if self.args.objc_ml_buckets:
            infer_options += ['-objc_ml_buckets', self.args.objc_ml_buckets]

        if self.args.notest:
            infer_options += ['-notest']

        if self.args.debug:
            infer_options += [
                '-developer_mode',
                '-html',
                '-dotty',
                '-print_types',
                '-trace_error',
                '-print_buckets',
                # '-notest',
            ]

        if self.args.specs_dirs:
            infer_options += self.args.specs_dirs

        exit_status = os.EX_OK

        if self.args.buck:
            infer_options += ['-project_root', os.getcwd(), '-java']
            if self.javac.args.classpath is not None:
                for path in self.javac.args.classpath.split(os.pathsep):
                    if os.path.isfile(path):
                        infer_options += ['-ziplib', os.path.abspath(path)]
        elif self.args.project_root:
            infer_options += ['-project_root', self.args.project_root]

        if self.args.multicore == 1:
            analyze_cmd = infer_analyze + infer_options
            exit_status = run_command(
                analyze_cmd,
                self.args.debug,
                self.javac.original_arguments,
                'analysis',
                self.args.analyzer
            )

        else:
            if self.args.analyzer in [ERADICATE, CHECKERS]:
                infer_analyze.append('-intraprocedural')

            os.environ['INFER_OPTIONS'] = ' '.join(infer_options)

            multicore_dir = os.path.join(self.args.infer_out, 'multicore')
            pwd = os.getcwd()
            if os.path.isdir(multicore_dir):
                shutil.rmtree(multicore_dir)
            os.mkdir(multicore_dir)
            os.chdir(multicore_dir)
            analyze_cmd = infer_analyze + ['-makefile', 'Makefile']
            analyze_cmd += infer_options
            makefile_status = run_command(
                analyze_cmd,
                self.args.debug,
                self.javac.original_arguments,
                'create_makefile',
                self.args.analyzer
            )
            exit_status += makefile_status
            if makefile_status == os.EX_OK:
                make_cmd = ['make', '-k', '-j', str(self.args.multicore)]
                if not self.args.debug:
                    make_cmd += ['-s']
                make_status = run_command(
                    make_cmd,
                    self.args.debug,
                    self.javac.original_arguments,
                    'run_makefile',
                    self.args.analyzer
                )
                os.chdir(pwd)
                exit_status += make_status

        if self.args.buck and exit_status == os.EX_OK:
            clean_infer_out(self.args.infer_out)

        cfgs = os.path.join(self.args.infer_out, 'captured', '*', '')
        captured_total = len(glob.glob(cfgs))
        captured_plural = '' if captured_total <= 1 else 's'
        print('\n%d file%s analyzed' % (captured_total, captured_plural))

        logging.info('Analyzed file count: %d', captured_total)
        logging.info('Analysis status: %d', exit_status)

        return exit_status

    def file_stats(self, file, stats):
        if file is not None:
            stats['files'] += 1
            try:
                with open(file, 'r') as f:
                    stats['lines'] += len(list(f))
            except IOError:
                logging.warning('File {} not found'.format(file))


    def javac_stats(self):
        stats = {'files': 0, 'lines': 0}

        for arg in self.javac.original_arguments:
            file = None
            if arg.endswith('.java'):
                file = arg
                self.file_stats(file, stats)
            if arg.startswith('@'):
                with open(arg[1:], 'r') as f:
                    for line in f:
                        file = line.strip()
                        self.file_stats(file, stats)

        return stats

    def update_stats(self, csv_report):
        with open(csv_report, 'r') as file_in:
            reader = csv.reader(file_in)
            rows = [row for row in reader][1:]
            for row in rows:
                key = row[utils.CSV_INDEX_TYPE]
                previous_value = self.stats['int'].get(key, 0)
                self.stats['int'][key] = previous_value + 1

    def report_errors(self):
        """Report statistics about the computation and create a CSV file
        containing the list or errors found during the analysis"""

        csv_report = os.path.join(self.args.infer_out,
                                  utils.CSV_REPORT_FILENAME)
        bugs_out = os.path.join(self.args.infer_out,
                                utils.BUGS_FILENAME)
        procs_report = os.path.join(self.args.infer_out, 'procs.csv')

        infer_print_cmd = [utils.get_cmd_in_bin_dir('InferPrint')]
        infer_print_options = [
            '-q',
            '-results_dir', self.args.infer_out,
            '-bugs', csv_report,
            '-procs', procs_report,
            '-analyzer', self.args.analyzer
        ]

        if self.args.specs_dirs:
            infer_print_options += self.args.specs_dirs

        exit_status = subprocess.check_call(
            infer_print_cmd + infer_print_options
        )
        if exit_status != os.EX_OK:
            logging.error('Error with InferPrint with the command: '
                          + infer_print_cmd)
        else:
            clean_csv(self.args, csv_report)
            self.update_stats(csv_report)
            utils.create_json_report(self.args.infer_out)

            print('\n')
            if not self.args.buck:
                print_errors(csv_report, bugs_out)

        return exit_status

    def save_stats(self):
        """Print timing information to infer_out/stats.json"""
        stats_path = os.path.join(self.args.infer_out, utils.STATS_FILENAME)

        self.stats['int'].update(self.javac_stats())

        self.stats['float'].update({
            'capture_time': self.timing.get('capture', 0.0),
            'analysis_time': self.timing.get('analysis', 0.0),
            'reporting_time': self.timing.get('reporting', 0.0),
        })

        # Adding the analyzer and the version of Infer
        self.stats['normal'] = {}
        self.stats['normal']['analyzer'] = self.args.analyzer
        self.stats['normal']['infer_version'] = utils.infer_version()

        with open(stats_path, 'w') as stats_file:
            json.dump(self.stats, stats_file, indent=2)

    def close(self):
        if self.args.analyzer != COMPILE:
            os.remove(self.javac.verbose_out)

    def analyze_and_report(self):
        if self.args.analyzer not in [COMPILE, CAPTURE]:
            analysis_start_time = time.time()
            if self.analyze() == os.EX_OK:
                elapsed = utils.elapsed_time(analysis_start_time)
                self.timing['analysis'] = elapsed
                reporting_start_time = time.time()
                self.report_errors()
                elapsed = utils.elapsed_time(reporting_start_time)
                self.timing['reporting'] = elapsed

    def start(self):
        if self.javac.args.version:
            if self.args.buck:
                key = self.args.analyzer
                print(utils.infer_key(key), file=sys.stderr)
            else:
                return self.javac.run()
        else:
            start_time = time.time()
            if self.capture() == os.EX_OK:
                self.timing['capture'] = utils.elapsed_time(start_time)
                self.analyze_and_report()
                self.close()
                elapsed = utils.elapsed_time(start_time)
                self.timing['total'] = elapsed
                self.save_stats()
                return self.stats
            else:
                return dict({})

# vim: set sw=4 ts=4 et:
