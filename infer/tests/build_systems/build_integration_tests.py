#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

# example usage:
# # run all the tests
# ./build_integration_test.py
# # run only the ant and gradle tests
# ./build_integration_test.py -- ant gradle
# # run no test
# ./build_integration_test.py --
# # run only the waf tests and record the output
# INFER_RECORD_INTEGRATION_TESTS=1 ./build_integration_test.py -- waf

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import json
import os
import platform
import shutil
import subprocess
import sys
import tempfile
import unittest

SCRIPT_DIR = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0,
                os.path.join(SCRIPT_DIR,
                             os.pardir, os.pardir, 'lib', 'python'))

from inferlib import config, issues, utils


ROOT_DIR = os.path.join(SCRIPT_DIR, os.pardir, os.pardir, os.pardir)

INFER_BIN = os.path.join(ROOT_DIR, 'infer', 'bin', 'infer')
INFERPRINT_BIN = os.path.join(ROOT_DIR, 'infer', 'bin', 'InferPrint')

CLANG_BIN = os.path.join(ROOT_DIR, 'facebook-clang-plugins', 'clang',
                         'install', 'bin', 'clang')

REPORT_JSON = 'report.json'

RECORD_ENV = 'INFER_RECORD_INTEGRATION_TESTS'

REPORT_FIELDS = [
    issues.JSON_INDEX_FILENAME,
    issues.JSON_INDEX_PROCEDURE,
    issues.JSON_INDEX_TYPE,
]

CODETOANALYZE_DIR = os.path.join(SCRIPT_DIR, 'codetoanalyze')
EXPECTED_OUTPUTS_DIR = os.path.join(SCRIPT_DIR, 'expected_outputs')

ALL_TESTS = [
    'waf',
]

to_test = ALL_TESTS


def should_record_tests():
    return RECORD_ENV in os.environ and os.environ[RECORD_ENV] == '1'


def quote(s):
    return '\"%s\"' % s


def string_of_error(e):
    line = ''
    if issues.JSON_INDEX_LINE in e:
        line = ' on line %s ' % e[issues.JSON_INDEX_LINE]
    msg = '%s in file %s, procedure %s%s' % (
        e[issues.JSON_INDEX_TYPE],
        quote(e[issues.JSON_INDEX_FILENAME]),
        quote(e[issues.JSON_INDEX_PROCEDURE]),
        line,
    )
    return msg


def save_report(reports, filename):
    # sorting to avoid spurious differences between two lists of reports
    reports.sort()

    def filter_report(report):
        return dict((k, v) for (k, v) in report.items() if k in REPORT_FIELDS)

    def should_report(report):
        return len(report) > 0

    filtered = filter(should_report, map(filter_report, reports))
    utils.dump_json_to_path(filtered, filename,
                            separators=(',', ': '), sort_keys=True)


def run_analysis(clean_cmds, build_cmds, extra_check, should_fail, env=None):
    for clean_cmd in clean_cmds:
        subprocess.check_call(clean_cmd, env=env)

    temp_out_dir = tempfile.mkdtemp(suffix='_out', prefix='infer_')
    for build_cmd in build_cmds:
        extra_args = (build_cmd['infer_args']
                      if 'infer_args' in build_cmd
                      else [])
        infer_cmd = ([INFER_BIN, '-o', temp_out_dir] +
                     extra_args +
                     ['--'] +
                     build_cmd['compile'])
        # Only record the output of the last build command. We record
        # all of them but each command overwrites the output of the
        # previous one.
        with tempfile.TemporaryFile(
                mode='w',
                suffix='.out',
                prefix='analysis_') as analysis_output:
            try:
                subprocess.check_call(infer_cmd,
                                      stdout=analysis_output, env=env)
                if should_fail is not None:
                    # hacky since we should clean up infer-out, etc. as below
                    # if you made the test fails, this is your punishment
                    assert False
            except subprocess.CalledProcessError, exn:
                if exn.returncode != should_fail:
                    raise

        # Set this to True to create an issues.exp file using the
        # results of the test. This is a temporary hack to aid
        # migrating the tests from this file to Makefiles. It can be
        # useful to compare the result of your migrated test with the
        # issues.exp that this gives you.
        if False:
            inferprint_cmd = (
                [INFERPRINT_BIN, '-q', '--issues-tests', 'issues.exp',
                 '--from-json-report',
                 os.path.join(temp_out_dir, 'report.json')] +
                extra_args)
            subprocess.check_call(inferprint_cmd, env=env)

    json_path = os.path.join(temp_out_dir, REPORT_JSON)
    found_errors = utils.load_json_from_path(json_path)
    extra_check(temp_out_dir)
    shutil.rmtree(temp_out_dir)
    os.chdir(SCRIPT_DIR)

    return found_errors


def match_pattern(f, p):
    for key in p.keys():
        if f[key] != p[key]:
            return False
    return True


def is_expected(e, patterns):
    for p in patterns:
        if match_pattern(e, p):
            return True
    return False


def is_missing(p, errors):
    for e in errors:
        if match_pattern(e, p):
            return False
    return True


def unexpected_errors(errors, patterns):
    return [e for e in errors if not is_expected(e, patterns)]


def missing_errors(errors, patterns):
    return [p for p in patterns if is_missing(p, errors)]


def check_results(errors, patterns):
    unexpected = unexpected_errors(errors, patterns)
    if unexpected != []:
        utils.stderr('\nInfer found the following unexpected errors:')
        for e in unexpected:
            utils.stderr('\t{}\n'.format(string_of_error(e)))
    missing = missing_errors(errors, patterns)
    if missing != []:
        utils.stderr('\nInfer did not find the following errors:')
        for p in missing:
            utils.stderr('\t{}\n'.format(string_of_error(p)))
    assert unexpected == []
    assert missing == []


def is_tool_available(cmd):
    try:
        with open(os.devnull, 'w') as devnull:
            subprocess.call(cmd, stdout=devnull)
    except OSError as e:
        if e.errno == os.errno.ENOENT:
            return False
        else:
            raise
    return True


def do_test(errors, expected_errors_filename):
    if should_record_tests():
        save_report(errors, expected_errors_filename)
        return
    else:
        patterns = utils.load_json_from_path(expected_errors_filename)
        check_results(errors, patterns)


def make_paths_relative_in_report(root, errors):
    for error in errors:
        # remove "root/" from each file name
        rel_fname = error[issues.JSON_INDEX_FILENAME][len(root) + 1:]
        error[issues.JSON_INDEX_FILENAME] = rel_fname
    return errors


def test(name,
         readable_name,
         root,
         compile_commands,
         clean_commands=[],
         env=None,
         available=lambda: True,
         enabled=None,
         report_fname=None,
         extra_check=lambda x: None,
         should_fail=None,
         preprocess=lambda: None,
         postprocess=lambda errors: errors):
    """Run a test.

    Arguments:
    - [name] is used to test if the test is enabled by default (but
      see [enabled])
    - [root] the directory from which to run the test
    - [compile_commands] the commands to be captured by Infer
    - [clean_commands] commands to setup the build directory prior to
      running Infer
    - [env] the environment in which to run all the commands
    - [available] a test to determine whether the test can be run
    - [enabled] whether the test should attempt to run. By default it
      is enabled if [[name] in [to_test]]
    - [report_fname] where to find the expected Infer results
    - [extra_check] some function that will be given the temporary
      results directory as argument
    - [should_fail] if not None then running infer is expected to fail
      with [should_fail] error code
    - [preprocess] a function to run before the clean and compile
      commands. If the function returns something non-None, use that as
      the compile commands.
    - [postprocess] a function that takes in an Infer report and can
      modify them. It must return an Infer report.

    Returns [True] if the test ran, [False] otherwise.

    """
    # python can't into using values of arguments in the default
    # values of other arguments
    if enabled is None:
        enabled = name in to_test
    if report_fname is None:
        report_fname = '%s_report.json' % name

    if not (enabled and available()):
        print('Skipping %s integration test' % readable_name)
        return False

    print('\nRunning %s integration test' % readable_name)

    if not os.path.exists(root):
        os.makedirs(root)
    os.chdir(root)

    pre = preprocess()
    if pre is not None:
        compile_commands = pre

    # rerun this in case preprocess() deleted the current directory
    if not os.path.exists(root):
        os.makedirs(root)
    os.chdir(root)

    errors = run_analysis(
        clean_commands,
        compile_commands,
        extra_check=extra_check,
        should_fail=should_fail,
        env=env)
    original = os.path.join(EXPECTED_OUTPUTS_DIR, report_fname)
    do_test(postprocess(errors), original)
    return True

class BuildIntegrationTest(unittest.TestCase):
    pass

if __name__ == '__main__':
    # hackish capturing of the arguments after '--'
    try:
        i = sys.argv.index('--')
        to_test = sys.argv[i + 1:]
        sys.argv = sys.argv[:i]
    except ValueError:
        pass

    unittest.main()  # run all the tests
