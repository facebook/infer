#!/usr/bin/env python2.7
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
# # run only the buck tests and record the output
# INFER_RECORD_INTEGRATION_TESTS=1 ./build_integration_test.py -- buck

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import json
import os
import shutil
import subprocess
import sys
import tempfile
import unittest

SCRIPT_DIR = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0,
                os.path.join(SCRIPT_DIR,
                             os.pardir, os.pardir, 'lib', 'python'))

from inferlib import issues, utils


ROOT_DIR = os.path.join(SCRIPT_DIR, os.pardir, os.pardir, os.pardir)

REPORT_JSON = 'report.json'

INFER_EXECUTABLE = 'infer'

RECORD_ENV = 'INFER_RECORD_INTEGRATION_TESTS'

REPORT_FIELDS = [
    issues.JSON_INDEX_FILENAME,
    issues.JSON_INDEX_PROCEDURE,
    issues.JSON_INDEX_TYPE,
]

CODETOANALYZE_DIR = os.path.join(SCRIPT_DIR, 'codetoanalyze')
EXPECTED_OUTPUTS_DIR = os.path.join(SCRIPT_DIR, 'expected_outputs')

ALL_TESTS = ['ant', 'buck', 'gradle', 'make', 'locale', 'waf']

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


def run_analysis(root, clean_cmds, build_cmds, analyzer, env=None):
    if not os.path.exists(root):
        os.makedirs(root)
    os.chdir(root)

    for clean_cmd in clean_cmds:
        subprocess.check_call(clean_cmd, env=env)

    for build_cmd in build_cmds:
        temp_out_dir = tempfile.mkdtemp(suffix='_out', prefix='infer_')
        infer_cmd = (['infer', '-a', analyzer, '-o', temp_out_dir, '--'] +
                     build_cmd)
        # Only record the output of the last build command. We record
        # all of them but each command overwrites the output of the
        # previous one.
        with tempfile.TemporaryFile(
                mode='w',
                suffix='.out',
                prefix='analysis_') as analysis_output:
            subprocess.check_call(infer_cmd, stdout=analysis_output, env=env)

    json_path = os.path.join(temp_out_dir, REPORT_JSON)
    found_errors = utils.load_json_from_path(json_path)
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
        subprocess.call(cmd)
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


class BuildIntegrationTest(unittest.TestCase):

    def test_ant_integration(self):
        if not ('ant' in to_test and is_tool_available(['ant', '-version'])):
            print('\nSkipping Ant integration test')
            return

        print('\nRunning Ant integration test')
        root = os.path.join(SCRIPT_DIR, os.pardir)
        errors = run_analysis(
            root,
            [['ant', 'clean']],
            [['ant', 'compile']],
            INFER_EXECUTABLE)
        original = os.path.join(EXPECTED_OUTPUTS_DIR, 'ant_report.json')
        do_test(errors, original)


    def test_gradle_integration(self):
        if 'gradle' not in to_test:
            print('\nSkipping Gradle integration test')
            return

        print('\nRunning Gradle integration test using mock gradle')
        root = os.path.join(ROOT_DIR, 'examples', 'java_hello')
        env = os.environ
        env['PATH'] = '{}:{}'.format(
            os.path.join(SCRIPT_DIR, 'mock'),
            os.getenv('PATH'),
        )
        errors = run_analysis(
            root,
            [],
            [['gradle', 'build']],
            INFER_EXECUTABLE,
            env=env)
        original = os.path.join(EXPECTED_OUTPUTS_DIR, 'gradle_report.json')
        do_test(errors, original)

    def test_buck_integration(self):
        if not ('buck' in to_test and
                is_tool_available(['buck', '--version'])):
            print('\nSkipping Buck integration test')
            return

        print('\nRunning Buck integration test')
        errors = run_analysis(
            ROOT_DIR,
            [['buck', 'clean']],
            [['buck', 'build', 'infer']],
            INFER_EXECUTABLE)
        original = os.path.join(EXPECTED_OUTPUTS_DIR, 'buck_report.json')
        do_test(errors, original)

    def test_make_integration(self):
        if 'make' not in to_test:
            print('\nSkipping make integration test')
            return

        print('\nRunning make integration test')
        root = os.path.join(CODETOANALYZE_DIR, 'make')
        errors = run_analysis(
            root,
            [['make', 'clean']],
            [['make', 'all']],
            INFER_EXECUTABLE)
        original = os.path.join(EXPECTED_OUTPUTS_DIR, 'make_report.json')
        do_test(errors, original)

    def test_wonky_locale_integration(self):
        if 'locale' not in to_test:
            print('\nSkipping wonky locale integration test')
            return

        print('\nRunning wonky locale integration test')
        root = os.path.join(CODETOANALYZE_DIR, 'make')
        env = os.environ
        env['LC_ALL'] = 'C'
        # check that we are able to remove the previous results by
        # running the analysis twice
        errors = run_analysis(
            root,
            [],
            [['clang', '-c', 'utf8_in_function_names.c'],
             ['clang', '-c', 'utf8_in_function_names.c']],
            INFER_EXECUTABLE,
            env=env)
        original = os.path.join(EXPECTED_OUTPUTS_DIR, 'locale_report.json')
        do_test(errors, original)

    def test_waf_integration(self):
        if 'waf' not in to_test:
            print('\nSkipping waf integration test')
            return

        print('\nRunning waf integration test')
        root = os.path.join(CODETOANALYZE_DIR, 'make')
        errors = run_analysis(
            root,
            [['make', 'clean']],
            [['./waf', 'build']],
            INFER_EXECUTABLE)
        original = os.path.join(EXPECTED_OUTPUTS_DIR, 'waf_report.json')
        do_test(errors, original)

    def test_cmake_integration(self):
        if not ('cmake' in to_test and
                is_tool_available(['cmake', '--version'])):
            print('\nSkipping cmake integration test')
            return

        print('\nRunning cmake integration test')
        root = os.path.join(CODETOANALYZE_DIR, 'cmake', 'build')
        errors = run_analysis(
            root,
            [],
            [['cmake', '..'], ['make', 'clean', 'all']],
            INFER_EXECUTABLE)
        # remove build/ directory
        shutil.rmtree(root)
        original = os.path.join(EXPECTED_OUTPUTS_DIR, 'cmake_report.json')
        do_test(errors, original)


if __name__ == '__main__':
    # hackish capturing of the arguments after '--'
    try:
        i = sys.argv.index('--')
        to_test = sys.argv[i + 1:]
        sys.argv = sys.argv[:i]
    except ValueError:
        pass

    unittest.main()  # run all the tests
