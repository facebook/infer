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
import codecs
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

from . import jwlib, utils

# Increase the limit of the CSV parser to sys.maxlimit
csv.field_size_limit(sys.maxsize)

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

BUG_FOUND_ERROR_CODE = 2

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


class ConfirmIncrementalAction(argparse._StoreTrueAction):
    def __call__(self, parser, namespace, values, option_string=None):
        if not getattr(namespace, 'incremental'):
            parser.error('-ic/--changed-only should only be used with -i')
        super(ConfirmIncrementalAction, self).__call__(parser,
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
                        help='''Analyze only changed procedures and their
                        dependencies''')
base_group.add_argument('-ic', '--changed-only',
                        action=ConfirmIncrementalAction,
                        help='''Same as -i, but does not analyze
                        dependencies of changed procedures.''')
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
base_group.add_argument('--fail-on-bug', action='store_true',
                        help='''Exit with error code %d if Infer found
                        something to report'''
                        % BUG_FOUND_ERROR_CODE)

base_group.add_argument('--android-harness', action='store_true',
                        help='''[experimental] Create harness to detect bugs
                        involving the Android lifecycle''')

base_group.add_argument('-l', '--llvm', action='store_true',
                        help='''[experimental] Analyze C or C++ file using the
                        experimental LLVM frontend''')

base_group.add_argument('--log_to_stderr', action='store_true',
                        help='''When set, all logging will go to stderr instead
                        of the log file''')
base_parser.add_argument('-v', '--version',
                         help='''Print the version of Infer and exit''',
                         action=VersionAction)


infer_parser = argparse.ArgumentParser(parents=[base_parser])
infer_group = infer_parser.add_argument_group('backend arguments')
infer_group.add_argument('-j', '--multicore', metavar='n', type=int,
                           default=multiprocessing.cpu_count(),
                           dest='multicore', help='Set the number of cores to '
                           'be used for the analysis (default uses all cores)')
infer_group.add_argument('-x', '--project', metavar='<projectname>',
                           help='Project name, for recording purposes only')

infer_group.add_argument('-r', '--revision', metavar='<githash>',
                           help='The githash, for recording purposes only')

infer_group.add_argument('--buck', action='store_true', dest='buck',
                           help='To use when run with buck')

infer_group.add_argument('--infer_cache', metavar='<directory>',
                           help='Select a directory to contain the infer cache')

infer_group.add_argument('-pr', '--project_root',
                          dest='project_root',
                          default=os.getcwd(),
                          help='Location of the project root '
                          '(default is current directory)')

infer_group.add_argument('--absolute-paths',
                          action='store_true',
                          default=False,
                          help='Report errors with absolute paths')

infer_group.add_argument('--ml_buckets',
                         dest='ml_buckets',
                         help='memory leak buckets to be checked, '
                              'separated by commas. The possible '
                              'buckets are cf (Core Foundation), '
                              'arc, narc (No arc), cpp')

infer_group.add_argument('-nt', '--notest', action='store_true',
                           dest='notest',
                           help='Prints output of symbolic execution')

infer_group.add_argument('-npb', '--no-progress-bar', action='store_true',
                         help='Do not show a progress bar in the analysis')

infer_group.add_argument('--specs-dir',
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


def get_infer_args(args):
    index = detect_javac(args)
    if index is None:
        cmd_args = args
    else:
        cmd_args = args[:index]
    return infer_parser.parse_args(cmd_args)


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


def clean(infer_out):
    directories = ['multicore', 'classnames', 'sources', jwlib.FILELISTS]
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
    infer_parser.print_usage()
    exit(1)


def compare_csv_rows(row_1, row_2):
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


def compare_json_rows(row_1, row_2):
    filename_1 = row_1[utils.JSON_INDEX_FILENAME]
    filename_2 = row_2[utils.JSON_INDEX_FILENAME]
    if filename_1 < filename_2:
        return -1
    elif filename_1 > filename_2:
        return 1
    else:
        line_1 = int(row_1[utils.JSON_INDEX_LINE])
        line_2 = int(row_2[utils.JSON_INDEX_LINE])
        return line_1 - line_2


def should_report(analyzer, error_kind, error_type, error_bucket):
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
        'ASSERTION_FAILURE',
        'CONTEXT_LEAK',
        'BAD_POINTER_COMPARISON',
        'TAINTED_VALUE_REACHING_SENSITIVE_FUNCTION',
        'STRONG_DELEGATE_WARNING',
        # 'CHECKERS_PRINTF_ARGS'
        # TODO (#8030397): revert this once all the checkers are moved to Infer
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


def should_report_csv(analyzer, row):
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

    return should_report(analyzer, error_kind, error_type, error_bucket)


def should_report_json(analyzer, row):
    error_kind = row[utils.JSON_INDEX_KIND]
    error_type = row[utils.JSON_INDEX_TYPE]
    error_bucket = ''  # can be updated later once we extract it from qualifier

    for qual_tag in row[utils.QUALIFIER_TAGS]:
        if qual_tag['tag'] == utils.BUCKET_TAGS:
            error_bucket = qual_tag['value']
            break

    return should_report(analyzer, error_kind, error_type, error_bucket)


def clean_json(args, json_report):
    collected_rows = []
    with codecs.open(json_report, 'r', encoding=utils.LOCALE) as file_in:
        rows = json.load(file_in)
        for row in rows:
            filename = row[utils.JSON_INDEX_FILENAME]
            if os.path.isfile(filename):
                if args.no_filtering or should_report_json(args.analyzer, row):
                    collected_rows.append(row)
        collected_rows = sorted(
            collected_rows,
            cmp=compare_json_rows)
    temporary_file = tempfile.mktemp()
    with codecs.open(temporary_file, 'w', encoding=utils.LOCALE) as file_out:
        json.dump(collected_rows, file_out)
        file_out.flush()
        shutil.move(temporary_file, json_report)


def clean_csv(args, csv_report):
    collected_rows = []
    with open(csv_report, 'r') as file_in:
        reader = utils.locale_csv_reader(file_in)
        rows = [row for row in reader]
        if len(rows) <= 1:
            return rows
        else:
            for row in rows[1:]:
                filename = row[utils.CSV_INDEX_FILENAME]
                if os.path.isfile(filename):
                    if args.no_filtering \
                       or should_report_csv(args.analyzer, row):
                        collected_rows.append(row)
            collected_rows = sorted(
                collected_rows,
                cmp=compare_csv_rows)
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
    with codecs.open(csv_report, 'r', encoding=utils.LOCALE) as file_in:
        reader = utils.locale_csv_reader(file_in)
        reader.next()  # first line is header, skip it

        errors = filter(
            lambda row: row[utils.CSV_INDEX_KIND] in [ERROR, WARNING],
            reader
        )

        with codecs.open(bugs_out, 'w', encoding=utils.LOCALE) as file_out:
            text_errors_list = []
            for row in errors:
                filename = row[utils.CSV_INDEX_FILENAME]
                if os.path.isfile(filename):
                    kind = row[utils.CSV_INDEX_KIND]
                    line = row[utils.CSV_INDEX_LINE]
                    error_type = row[utils.CSV_INDEX_TYPE]
                    msg = row[utils.CSV_INDEX_QUALIFIER]
                    indenter = utils.Indenter()
                    indenter.indent_push()
                    indenter.add(
                        utils.build_source_context(filename,
                                                   utils.TERMINAL_FORMATTER,
                                                   int(line)))
                    source_context = unicode(indenter)
                    text_errors_list.append(
                        u'{0}:{1}: {2}: {3}\n  {4}\n{5}'.format(
                            filename,
                            line,
                            kind.lower(),
                            error_type,
                            msg,
                            source_context,
                        )
                    )
            n_issues = len(text_errors_list)
            if n_issues == 0:
                print_and_write(file_out, 'No issues found')
            else:
                msg = '\nFound %s\n' % utils.get_plural('issue', n_issues)
                print_and_write(file_out, msg)
                text_errors = '\n\n'.join(text_errors_list)
                print_and_write(file_out, text_errors)


def run_command(cmd, debug_mode, javac_arguments, step, analyzer):
    if debug_mode:
        print('\n{0}\n'.format(' '.join(cmd)))
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

            self.stats = {'int': {}}
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

        if not self.args.absolute_paths:
            infer_cmd += ['-project_root', self.args.project_root]

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
        if self.args.android_harness:
            infer_cmd.append('-harness')

        return run_command(
            infer_cmd,
            self.args.debug,
            self.javac.original_arguments,
            'frontend',
            self.args.analyzer
        )

    def compile(self):
        return self.javac.run()

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

        if self.args.ml_buckets:
            infer_options += ['-ml_buckets', self.args.ml_buckets]

        if self.args.notest:
            infer_options += ['-notest']

        if self.args.no_progress_bar:
            infer_options += ['-no_progress_bar']

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

        if self.args.incremental:
            if self.args.changed_only:
                infer_options.append('-incremental_changed_only')
            else:
                infer_options.append('-incremental')

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
            analysis_start_time = time.time()
            analyze_cmd = infer_analyze + infer_options
            exit_status = run_command(
                analyze_cmd,
                self.args.debug,
                self.javac.original_arguments,
                'analysis',
                self.args.analyzer
            )
            elapsed = utils.elapsed_time(analysis_start_time)
            self.timing['analysis'] = elapsed
            self.timing['makefile_generation'] = 0

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
            makefile_generation_start_time = time.time()
            makefile_status = run_command(
                analyze_cmd,
                self.args.debug,
                self.javac.original_arguments,
                'create_makefile',
                self.args.analyzer
            )
            elapsed = utils.elapsed_time(makefile_generation_start_time)
            self.timing['makefile_generation'] = elapsed
            exit_status += makefile_status
            if makefile_status == os.EX_OK:
                make_cmd = ['make', '-k', '-j', str(self.args.multicore)]
                if not self.args.debug:
                    make_cmd += ['-s']
                analysis_start_time = time.time()
                make_status = run_command(
                    make_cmd,
                    self.args.debug,
                    self.javac.original_arguments,
                    'run_makefile',
                    self.args.analyzer
                )
                elapsed = utils.elapsed_time(analysis_start_time)
                self.timing['analysis'] = elapsed
                os.chdir(pwd)
                exit_status += make_status

        if self.args.buck and exit_status == os.EX_OK:
            clean(self.args.infer_out)

        return exit_status

    def update_stats_with_warnings(self, csv_report):
        with open(csv_report, 'r') as file_in:
            reader = utils.locale_csv_reader(file_in)
            rows = [row for row in reader][1:]
            for row in rows:
                key = row[utils.CSV_INDEX_TYPE]
                previous_value = self.stats['int'].get(key, 0)
                self.stats['int'][key] = previous_value + 1

    def create_report(self):
        """Report statistics about the computation and create a CSV file
        containing the list or errors found during the analysis"""

        out_dir = self.args.infer_out
        csv_report = os.path.join(out_dir, utils.CSV_REPORT_FILENAME)
        json_report = os.path.join(out_dir, utils.JSON_REPORT_FILENAME)
        procs_report = os.path.join(self.args.infer_out, 'procs.csv')

        infer_print_cmd = [utils.get_cmd_in_bin_dir('InferPrint')]
        infer_print_options = [
            '-q',
            '-results_dir', self.args.infer_out,
            '-bugs', csv_report,
            '-bugs_json', json_report,
            '-procs', procs_report,
            '-analyzer', self.args.analyzer
        ]
        if self.javac.annotations_out is not None:
            infer_print_options += [
                '-local_config', self.javac.annotations_out]
        exit_status = subprocess.check_call(
            infer_print_cmd + infer_print_options
        )
        if exit_status != os.EX_OK:
            logging.error('Error with InferPrint with the command: '
                          + infer_print_cmd)
        else:
            clean_csv(self.args, csv_report)
            clean_json(self.args, json_report)
            self.update_stats_with_warnings(csv_report)

        return exit_status

    def read_proc_stats(self):
        proc_stats_path = os.path.join(
            self.args.infer_out,
            utils.PROC_STATS_FILENAME)

        # capture and compile mode do not create proc_stats.json
        if os.path.isfile(proc_stats_path):
            with codecs.open(proc_stats_path, 'r',
                             encoding=utils.LOCALE) as proc_stats_file:
                proc_stats = json.load(proc_stats_file)
                self.stats['int'].update(proc_stats)

    def save_stats(self):
        """Print timing information to infer_out/stats.json"""
        self.stats['float'] = {
            'capture_time': self.timing.get('capture', 0.0),
            'makefile_generation_time': self.timing.get(
                'makefile_generation', 0.0),
            'analysis_time': self.timing.get('analysis', 0.0),
            'reporting_time': self.timing.get('reporting', 0.0),
        }
        self.stats['normal'] = {
            'analyzer': self.args.analyzer,
            'infer_version': utils.infer_version()
        }

        stats_path = os.path.join(self.args.infer_out, utils.STATS_FILENAME)
        with codecs.open(stats_path, 'w', encoding=utils.LOCALE) as stats_file:
            json.dump(self.stats, stats_file, indent=2)


    def close(self):
        os.remove(self.javac.verbose_out)
        os.remove(self.javac.annotations_out)

    def analyze_and_report(self):
        should_print_errors = False
        if self.args.analyzer not in [COMPILE, CAPTURE]:
            if self.analyze() == os.EX_OK:
                reporting_start_time = time.time()
                report_status = self.create_report()
                elapsed = utils.elapsed_time(reporting_start_time)
                self.timing['reporting'] = elapsed
                self.read_proc_stats()
                self.print_analysis_stats()
                if report_status == os.EX_OK and not self.args.buck:
                    csv_report = os.path.join(self.args.infer_out,
                                              utils.CSV_REPORT_FILENAME)
                    bugs_out = os.path.join(self.args.infer_out,
                                            utils.BUGS_FILENAME)
                    print_errors(csv_report, bugs_out)

    def print_analysis_stats(self):
        procs_total = self.stats['int']['procedures']
        files_total = self.stats['int']['files']
        procs_str = utils.get_plural('procedure', procs_total)
        files_str = utils.get_plural('file', files_total)
        print('Analyzed %s in %s' % (procs_str, files_str))

    def start(self):
        if self.javac.args.version:
            if self.args.buck:
                key = self.args.analyzer
                print(utils.infer_key(key), file=sys.stderr)
            else:
                return self.javac.run()
        else:
            start_time = time.time()

            self.compile()
            if self.args.analyzer == COMPILE:
                return os.EX_OK

            self.run_infer_frontend()
            self.timing['capture'] = utils.elapsed_time(start_time)
            if self.args.analyzer == CAPTURE:
                return os.EX_OK

            self.analyze_and_report()
            self.close()
            self.timing['total'] = utils.elapsed_time(start_time)
            self.save_stats()

            return self.stats

# vim: set sw=4 ts=4 et:
