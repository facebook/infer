#
# Copyright (c) 2013- Facebook.  All rights reserved.
#

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import csv
import fnmatch
import gzip
import json
import logging
import os
import subprocess
import sys
import tempfile
import time


BIN_DIRECTORY = os.path.dirname(os.path.realpath(__file__))
LIB_DIRECTORY = os.path.join(BIN_DIRECTORY, '..', 'lib', 'java')
TMP_DIRECTORY = tempfile.gettempdir()
MODELS_JAR = os.path.join(LIB_DIRECTORY, 'models.jar')


DEFAULT_INFER_OUT = os.path.join(os.getcwd(), 'infer-out')
CSV_PERF_FILENAME = 'performances.csv'
STATS_FILENAME = 'stats.json'

CSV_REPORT_FILENAME = 'report.csv'
JSON_REPORT_FILENAME = 'report.json'
BUGS_FILENAME = 'bugs.txt'

CSV_INDEX_KIND = 1
CSV_INDEX_TYPE = 2
CSV_INDEX_QUALIFIER = 3
CSV_INDEX_LINE = 5
CSV_INDEX_FILENAME = 8
CSV_INDEX_QUALIFIER_TAGS = 11

QUALIFIER_TAGS = 'qualifier_tags'
BUCKET_TAGS = 'bucket'

IOS_CAPTURE_ERRORS = 'errors'
IOS_BUILD_OUTPUT = 'build_output'

BUCK_INFER_OUT = 'infer'

FORMAT = '[%(levelname)s] %(message)s'
DEBUG_FORMAT = '[%(levelname)s:%(filename)s:%(lineno)03d] %(message)s'


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
    # this relies on the fact that utils.py is located in infer/bin
    return os.path.join(
        os.path.dirname(os.path.realpath(__file__)),
        binary_name)


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


def create_json_report(out_dir):
    csv_report_filename = os.path.join(out_dir, CSV_REPORT_FILENAME)
    json_report_filename = os.path.join(out_dir, JSON_REPORT_FILENAME)
    rows = []
    with open(csv_report_filename, 'r') as file_in:
        reader = csv.reader(file_in)
        rows = [row for row in reader]
    with open(json_report_filename, 'w') as file_out:
        headers = rows[0]
        issues = rows[1:]
        json.dump([dict(zip(headers, row)) for row in issues], file_out)

# vim: set sw=4 ts=4 et:
