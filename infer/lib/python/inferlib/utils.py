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
import fnmatch
import gzip
import json
import logging
import os
import re
try:
    import pygments
    import pygments.formatters
    import pygments.lexers
except ImportError:
    pygments = None
import subprocess
import sys
import tempfile
import time


# this assumes that this file lives in infer/lib/python/infer/ and the binaries
# are in infer/bin/
INFER_PYTHON_DIRECTORY = os.path.dirname(os.path.realpath(__file__))
INFER_ROOT_DIRECTORY = os.path.join(INFER_PYTHON_DIRECTORY,
                                    os.pardir, os.pardir, os.pardir, os.pardir)
INFER_INFER_DIRECTORY = os.path.join(INFER_ROOT_DIRECTORY, 'infer')
FCP_DIRECTORY = os.path.join(INFER_ROOT_DIRECTORY, 'facebook-clang-plugins')
LIB_DIRECTORY = os.path.join(INFER_INFER_DIRECTORY, 'lib')
BIN_DIRECTORY = os.path.join(INFER_INFER_DIRECTORY, 'bin')
TMP_DIRECTORY = tempfile.gettempdir()
JAVA_LIB_DIRECTORY = os.path.join(LIB_DIRECTORY, 'java')
MODELS_JAR = os.path.join(JAVA_LIB_DIRECTORY, 'models.jar')
ANNOT_PROCESSOR_JAR = os.path.join(JAVA_LIB_DIRECTORY, 'processor.jar')
WRAPPERS_DIRECTORY = os.path.join(LIB_DIRECTORY, 'wrappers')
XCODE_WRAPPERS_DIRECTORY = os.path.join(LIB_DIRECTORY, 'xcode_wrappers')

DEFAULT_INFER_OUT = os.path.join(os.getcwd(), 'infer-out')
CSV_PERF_FILENAME = 'performances.csv'
STATS_FILENAME = 'stats.json'
PROC_STATS_FILENAME = 'proc_stats.json'

CSV_REPORT_FILENAME = 'report.csv'
JSON_REPORT_FILENAME = 'report.json'
BUGS_FILENAME = 'bugs.txt'

# indices in rows of csv reports
CSV_INDEX_CLASS = 0
CSV_INDEX_KIND = 1
CSV_INDEX_TYPE = 2
CSV_INDEX_QUALIFIER = 3
CSV_INDEX_SEVERITY = 4
CSV_INDEX_LINE = 5
CSV_INDEX_PROCEDURE = 6
CSV_INDEX_PROCEDURE_ID = 7
CSV_INDEX_FILENAME = 8
CSV_INDEX_TRACE = 9
CSV_INDEX_KEY = 10
CSV_INDEX_QUALIFIER_TAGS = 11
CSV_INDEX_HASH = 12
CSV_INDEX_BUG_ID = 13
CSV_INDEX_ALWAYS_REPORT = 14
CSV_INDEX_ADVICE = 15

# field names in rows of json reports
JSON_INDEX_FILENAME = 'file'
JSON_INDEX_HASH = 'hash'
JSON_INDEX_KIND = 'kind'
JSON_INDEX_LINE = 'line'
JSON_INDEX_PROCEDURE = 'procedure'
JSON_INDEX_QUALIFIER = 'qualifier'
JSON_INDEX_QUALIFIER_TAGS = 'qualifier_tags'
JSON_INDEX_SEVERITY = 'file'
JSON_INDEX_TYPE = 'type'
JSON_INDEX_TRACE = 'trace'
JSON_INDEX_TRACE_LEVEL = 'level'
JSON_INDEX_TRACE_FILENAME = 'filename'
JSON_INDEX_TRACE_LINE = 'line_number'
JSON_INDEX_TRACE_DESCRIPTION = 'description'
JSON_INDEX_TRACE_NODE_TAGS = 'node_tags'
JSON_INDEX_TRACE_NODE_TAGS_TAG = 'tags'
JSON_INDEX_TRACE_NODE_TAGS_VALUE = 'value'

QUALIFIER_TAGS = 'qualifier_tags'
BUCKET_TAGS = 'bucket'

IOS_CAPTURE_ERRORS = 'errors'
IOS_BUILD_OUTPUT = 'build_output'

BUCK_INFER_OUT = 'infer'

FORMAT = '[%(levelname)s] %(message)s'
DEBUG_FORMAT = '[%(levelname)s:%(filename)s:%(lineno)03d] %(message)s'

BASE_INDENT = 2
# how many lines of context around each report
SOURCE_CONTEXT = 2

# syntax highlighting modes
PLAIN_FORMATTER = 0
TERMINAL_FORMATTER = 1


# Monkey patching subprocess (I'm so sorry!).
if "check_output" not in dir(subprocess):
    def f(*popenargs, **kwargs):
        if 'stdout' in kwargs:
            raise ValueError('stdout not supported')
        process = subprocess.Popen(
            stdout=subprocess.PIPE,
            *popenargs,
            **kwargs)
        output, unused_err = process.communicate()
        retcode = process.poll()
        if retcode:
            cmd = kwargs.get("args")
            if cmd is None:
                cmd = popenargs[0]
            raise subprocess.CalledProcessError(retcode, cmd)
        return output
    subprocess.check_output = f


def configure_logging(debug, quiet=False):
    """Configures the default logger. This can be called only once and has to
    be called before any logging is done.
    """
    logging.TIMING = logging.ERROR + 5
    logging.addLevelName(logging.TIMING, "TIMING")

    def timing(msg, *args, **kwargs):
        logging.log(logging.TIMING, msg, *args, **kwargs)

    logging.timing = timing
    if quiet:
        logging.basicConfig(level=logging.TIMING, format=FORMAT)
    elif not debug:
        logging.basicConfig(level=logging.INFO, format=FORMAT)
    else:
        logging.basicConfig(level=logging.DEBUG, format=DEBUG_FORMAT)


def elapsed_time(start_time):
    return time.time() - start_time


def error(msg):
    print(msg, file=sys.stderr)


def get_cmd_in_bin_dir(binary_name):
    return os.path.join(BIN_DIRECTORY, binary_name)


def write_cmd_streams_to_file(logfile, cmd=None, out=None, err=None):
    with open(logfile, 'w') as log_filedesc:
        if cmd:
            log_filedesc.write(' '.join(cmd) + '\n')
        if err is not None:
            errors = str(err)
            log_filedesc.write('\nSTDERR:\n')
            log_filedesc.write(errors)
        if out is not None:
            output = str(out)
            log_filedesc.write('\n\nSTDOUT:\n')
            log_filedesc.write(output)


def save_failed_command(
        infer_out,
        cmd,
        message,
        prefix='failed_',
        out=None,
        err=None):
    cmd_filename = tempfile.mktemp(
        '_' + message + ".txt",
        prefix, infer_out
    )
    write_cmd_streams_to_file(cmd_filename, cmd=cmd, out=out, err=err)
    logging.error('\n' + message + ' error saved in ' + cmd_filename)


def run_command(cmd, debug_mode, infer_out, message, env=os.environ):
    if debug_mode:
        print('\n{0}\n'.format(' '.join(cmd)))
    try:
        return subprocess.check_call(cmd, env=env)
    except subprocess.CalledProcessError as e:
        save_failed_command(infer_out, cmd, message)
        raise e


def print_exit(s):
    print(s)
    exit(os.EX_OK)


def infer_version():
    version = json.loads(subprocess.check_output([
        get_cmd_in_bin_dir('InferAnalyze'),
        '-version_json',
    ]).decode())
    return version['commit']


def infer_branch():
    version = json.loads(subprocess.check_output([
        get_cmd_in_bin_dir('InferAnalyze'),
        '-version_json',
    ]).decode())
    return version['branch']


def infer_key(analyzer):
    return os.pathsep.join([analyzer, infer_version()])


def vcs_branch(dir='.'):
    cwd = os.getcwd()
    devnull = open(os.devnull, 'w')
    try:
        os.chdir(dir)

        branch = subprocess.check_output(
            ['git',
             'rev-parse',
             '--abbrev-ref',
             'HEAD'],
            stderr=devnull).decode().strip()
    except subprocess.CalledProcessError:
        try:
            branch = subprocess.check_output(
                ['hg',
                 'id',
                 '-B'],
                stderr=devnull).decode().strip()
        except subprocess.CalledProcessError:
            branch = 'not-versioned'
    finally:
        devnull.close()
        os.chdir(cwd)
    return branch


def vcs_revision(dir='.'):
    cwd = os.getcwd()
    devnull = open(os.devnull, 'w')
    try:
        os.chdir(dir)

        revision = subprocess.check_output(
            ['git',
             'rev-parse',
             'HEAD'],
            stderr=devnull).decode().strip()
    except subprocess.CalledProcessError:
        try:
            revision = subprocess.check_output(
                ['hg',
                 'id',
                 '-i'],
                stderr=devnull).decode().strip()
        except subprocess.CalledProcessError:
            revision = 'not-versioned'
    finally:
        devnull.close()
        os.chdir(cwd)
    return revision


class Timer:
    """Simple logging timer. Initialize with a printf like logging function."""
    def __init__(self, logger=lambda x: None):
        self._logger = logger
        self._start = 0

    def start(self, message=None, *args):
        self._start = time.time()
        if message:
            self._logger(message, *args)

    def stop(self, message=None, *args):
        self._stop = time.time()
        self._dt = self._stop - self._start
        if message:
            self._logger(message + ' (%.2fs)', *(args + (self._dt,)))
        return self._dt



def interact():
    """Start interactive mode. Useful for debugging.
    """
    import code
    code.interact(local=locals())


def search_files(root_dir, extension):
    # Input:
    #   - root directory where to start a recursive search of yjson files
    #   - file extension to search from the root
    # Output:
    #   - list of absolute filepaths
    files = []
    if not os.path.isabs(root_dir):
        root_dir = os.path.abspath(root_dir)
    for dirpath, _, filenames in os.walk(root_dir):
        for filename in fnmatch.filter(filenames, "*" + extension):
            files.append(os.path.join(dirpath, filename))
    return files


def uncompress_gzip_file(gzip_file, out_dir):
    # This is python2.6 compliant, gzip.open doesn't support 'with' statement
    # Input:
    #    - gzip file path
    #    - output directory where uncompress the file
    # Output:
    #    - path of the uncompressed file
    #    NOTE: the file is permanently created, is responsibility of the
    #    caller to delete it
    uncompressed_path = None
    uncompressed_fd = None
    compressed_fd = None
    try:
        # the uncompressed filename loses its final extension
        # (for example abc.gz -> abc)
        uncompressed_path = os.path.join(
            out_dir,
            os.path.splitext(gzip_file)[0],
        )
        uncompressed_fd = open(uncompressed_path, 'wb')
        compressed_fd = gzip.open(gzip_file, 'rb')
        uncompressed_fd.write(compressed_fd.read())
        return uncompressed_path
    except IOError as exc:
        # delete the uncompressed file (if exists)
        if uncompressed_path is not None and os.path.exists(uncompressed_path):
            os.remove(uncompressed_path)
        raise exc
    finally:
        if compressed_fd is not None:
            compressed_fd.close()
        if uncompressed_fd is not None:
            uncompressed_fd.close()


def run_process(cmd, cwd=None, logfile=None):
    # Input:
    #    - command to execute
    #    - current working directory to cd before running the cmd
    #    - logfile where to dump stdout/stderr
    # Output:
    #    - exitcode of the executed process
    p = subprocess.Popen(
        cmd,
        cwd=cwd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)
    (out, err) = p.communicate()
    if logfile:
        write_cmd_streams_to_file(logfile, cmd=cmd, out=out, err=err)
    return p.returncode


def invoke_function_with_callbacks(
        func,
        args,
        on_terminate=None,
        on_exception=None):
    try:
        res = func(*args)
        if on_terminate:
            on_terminate(res)
        return res
    except Exception as exc:
        if on_exception:
            return on_exception(exc)
        raise


def save_as_json(data, filename):
    with open(filename, 'w') as file_out:
        json.dump(data, file_out, indent=2)


def merge_json_reports(report_paths, merged_report_path):
    # TODO: use streams instead of loading the entire json in memory
    json_data = []
    for json_path in report_paths:
        with open(json_path, 'r') as fd:
            json_data = json_data + json.loads(fd.read())
    save_as_json(json_data, merged_report_path)


def create_json_report(out_dir):
    csv_report_filename = os.path.join(out_dir, CSV_REPORT_FILENAME)
    json_report_filename = os.path.join(out_dir, JSON_REPORT_FILENAME)
    rows = []
    with open(csv_report_filename, 'r') as file_in:
        reader = csv.reader(file_in)
        rows = [row for row in reader]
    headers = rows[0]
    issues = [dict(zip(headers, row)) for row in rows[1:]]
    save_as_json(issues, json_report_filename)


def get_plural(_str, count):
    plural_str = _str if count == 1 else _str + 's'
    return '%d %s' % (count, plural_str)


class AbsolutePathAction(argparse.Action):
    """Convert a path from relative to absolute in the arg parser"""
    def __call__(self, parser, namespace, values, option_string=None):
        setattr(namespace, self.dest, os.path.abspath(values))


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
        lines = x.splitlines()
        indent = self.indent_get()
        lines = [indent + l for l in lines]
        self.text += '\n'.join(lines)

    def __str__(self):
        return self.text


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


def build_source_context(source_name, mode, report_line):
    start_line = max(1, report_line - SOURCE_CONTEXT)
    # could go beyond last line, checked in the loop
    end_line = report_line + SOURCE_CONTEXT

    n_length = len(str(end_line))
    line_number = 1
    s = ''
    with open(source_name) as source_file:
        for line in source_file:
            if start_line <= line_number <= end_line:
                num = str(line_number).zfill(n_length)
                caret = '  '
                if line_number == report_line:
                    caret = '> '
                s += num + '. ' + caret + line
            line_number += 1
    return syntax_highlighting(source_name, mode, s)

# vim: set sw=4 ts=4 et:
