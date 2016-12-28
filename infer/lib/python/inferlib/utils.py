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
import fnmatch
import gzip
import json
import logging
import os
import subprocess
import sys
import time

from . import config


FORMAT = '[%(levelname)s] %(message)s'
DEBUG_FORMAT = '[%(levelname)s:%(filename)s:%(lineno)03d] %(message)s'


# Monkey patching subprocess (I'm so sorry!).
if 'check_output' not in dir(subprocess):
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
            cmd = kwargs.get('args')
            if cmd is None:
                cmd = popenargs[0]
            raise subprocess.CalledProcessError(retcode, cmd)
        return output
    subprocess.check_output = f


# csv.reader() doesn't support utf-8. Do not use csv.reader(). Use
# this instead.
def locale_csv_reader(iterable, dialect='excel', **kwargs):
    rows = csv.reader(iterable, dialect=dialect, **kwargs)
    for row in rows:
        yield [unicode(cell, config.CODESET) for cell in row]


def configure_logging(args):
    """Configures the default logger. This can be called only once and has to
    be called before any logging is done.
    """
    logging.TIMING = logging.ERROR + 5
    logging.addLevelName(logging.TIMING, 'TIMING')

    def timing(msg, *args, **kwargs):
        logging.log(logging.TIMING, msg, *args, **kwargs)

    logging.timing = timing
    if args.debug:
        logging.basicConfig(level=logging.DEBUG, format=DEBUG_FORMAT)
    else:
        logging.basicConfig(level=logging.INFO,
                            format=FORMAT,
                            filename=os.path.join(args.infer_out,
                                                  config.LOG_FILE),
                            filemode='w')


def elapsed_time(start_time):
    return time.time() - start_time


def get_cmd_in_bin_dir(binary_name):
    return os.path.join(config.BIN_DIRECTORY, binary_name)


def load_json_from_path(path, errors='replace'):
    with codecs.open(path, 'r',
                     encoding=config.CODESET, errors=errors) as file_in:
        return json.load(file_in, encoding=config.CODESET)


def dump_json_to_path(
        data, path,
        skipkeys=False, ensure_ascii=True, check_circular=True, allow_nan=True,
        cls=None,
        indent=2,  # customized
        separators=None,
        encoding=config.CODESET,  # customized
        default=None, sort_keys=False, **kw):
    with codecs.open(path, 'w',
                     encoding=config.CODESET, errors='replace') as file_out:
        json.dump(data, file_out, skipkeys=skipkeys, ensure_ascii=ensure_ascii,
                  check_circular=check_circular, allow_nan=allow_nan, cls=cls,
                  indent=indent, separators=separators, encoding=encoding,
                  default=default, sort_keys=sort_keys, **kw)


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
    return '/'.join([analyzer, infer_version()])


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


def mkdir_if_not_exists(path):
    try:
        os.mkdir(path)
    except OSError:
        pass


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
        for filename in fnmatch.filter(filenames, '*' + extension):
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


def get_plural(_str, count):
    plural_str = _str if count == 1 else _str + 's'
    return '%d %s' % (count, plural_str)


def decode(s, errors='replace'):
    return s.decode(encoding=config.CODESET, errors=errors)


def encode(u, errors='replace'):
    return u.encode(encoding=config.CODESET, errors=errors)


def decode_or_not(s, errors='replace'):
    try:
        return decode(s, errors)
    except UnicodeEncodeError:
        return s


def encode_or_not(u, errors='replace'):
    try:
        return encode(u, errors)
    except UnicodeDecodeError:
        return u


def stdout(s, errors='replace'):
    print(encode(s, errors=errors))


def stderr(s, errors='replace'):
    print(encode(s, errors=errors), file=sys.stderr)


def merge_and_dedup_files_into_path(files_to_merge, dest):
    lines = set()
    for file_to_merge in files_to_merge:
        with open(file_to_merge, 'r') as fsrc:
            lines |= set(fsrc.readlines())
    with open(dest, 'w') as fdest:
        fdest.writelines(lines)


def read_env():
    env = dict(os.environ).copy()
    for k, v in env.iteritems():
        env[k] = decode(v)
    return env


def encode_env(env):
    new_env = env.copy()
    for k, v in new_env.iteritems():
        new_env[k] = encode(v)
    return new_env


class AbsolutePathAction(argparse.Action):
    """Convert a path from relative to absolute in the arg parser"""
    def __call__(self, parser, namespace, values, option_string=None):
        setattr(namespace, self.dest, encode(os.path.abspath(values)))
