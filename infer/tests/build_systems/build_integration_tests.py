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
    'cmake',
    'componentkit_analytics',
    'componentkit_imports',
    'delete',
    'fail',
    'gradle',
    'javac',
    'locale',
    'make',
    'multiclang',
    'ndk-build',
    'pmd-xml',
    'reactive',
    'unknown_ext',
    'utf8_in_pwd',
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

    def test_javac_integration(
            self,
            enabled=None,
            root=os.path.join(ROOT_DIR, 'examples'),
            report_fname='javac_report.json'):
        test('javac', 'javac',
             root,
             [{'compile': ['javac', 'Hello.java']}],
             enabled=enabled,
             report_fname=report_fname)

    def test_gradle_integration(
            self,
            enabled=None,
            root=os.path.join(ROOT_DIR, 'examples', 'java_hello'),
            report_fname='gradle_report.json'):
        env = os.environ.copy()
        env['PATH'] = '{}:{}'.format(
            os.path.join(SCRIPT_DIR, 'mock'),
            os.getenv('PATH'),
        )
        test('gradle', 'Gradle',
             root,
             [{'compile': ['gradle', 'build']}],
             enabled=enabled,
             report_fname=report_fname,
             env=env)

    def test_make_integration(
            self,
            enabled=None,
            root=os.path.join(CODETOANALYZE_DIR, 'make'),
            report_fname='make_report.json'):
        test('make', 'make',
             root,
             [{'compile': ['make', 'all']}],
             clean_commands=[['make', 'clean']],
             enabled=enabled,
             report_fname=report_fname)

    def test_ndkbuild_integration(self):
        root = os.path.join(CODETOANALYZE_DIR, 'ndk-build', 'hello_app')
        gen_lib_dir = os.path.join(root, 'libs')
        gen_obj_dir = os.path.join(root, 'obj')
        env = os.environ.copy()
        ndk_dir = os.getenv('ANDROID_NDK',
                            os.path.join(os.path.sep,
                                         'opt',
                                         'android_ndk',
                                         'r10e'))
        env['PATH'] = '{}:{}'.format(os.getenv('PATH'), ndk_dir)
        if test('ndk-build', 'ndk-build',
                root,
                [{'compile': ['ndk-build', '-B',
                              'NDK_LIBS_OUT=./libs', 'NDK_OUT=./obj']}],
                clean_commands=[['ndk-build', 'clean']],
                available=lambda: is_tool_available([
                    os.path.join(ndk_dir, 'ndk-build'), '-v']),
                env=env):
            # remove libs/ and obj/ directories
            shutil.rmtree(gen_lib_dir)
            shutil.rmtree(gen_obj_dir)

    def test_wonky_locale_integration(self):
        env = os.environ.copy()
        env['LC_ALL'] = 'C'
        test('locale', 'wonky locale',
             os.path.join(CODETOANALYZE_DIR, 'make'),
             [{'compile': ['clang', '-c', 'utf8_in_function_names.c']},
              {'compile': ['clang', '-c', 'utf8_in_function_names.c']}],
             env=env)

    def test_waf_integration(self):
        test('waf', 'waf',
             os.path.join(CODETOANALYZE_DIR, 'make'),
             [{'compile': ['./waf', 'build']}],
             clean_commands=[['make', 'clean']])

    def test_cmake_integration(
            self,
            enabled=None,
            root=os.path.join(CODETOANALYZE_DIR, 'cmake'),
            report_fname='cmake_report.json'):
        build_root = os.path.join(root, 'build')
        if test('cmake', 'CMake',
                build_root,
                [{'compile': ['cmake', '..']},
                 {'compile': ['make', 'clean', 'all']}],
                available=lambda: is_tool_available(['cmake', '--version']),
                enabled=enabled,
                # remove build/ directory just in case
                preprocess=lambda: shutil.rmtree(build_root, True),
                # cmake produces absolute paths using the real path
                postprocess=(lambda errors:
                             make_paths_relative_in_report(
                                 os.path.realpath(root), errors))):
            # remove build/ directory
            shutil.rmtree(build_root)

    def test_utf8_in_pwd_integration(self):
        if not 'utf8_in_pwd' in to_test:
            print('\nSkipping utf8_in_pwd integration test')
            return
        print('\nRunning utf8_in_pwd integration test')

        utf8_in_pwd_path = os.path.join(CODETOANALYZE_DIR, u'utf8_\u03B9n_pwd')

        # copy non-unicode dir to one with unicode in it
        shutil.rmtree(utf8_in_pwd_path, True) # remove just in case
        shutil.copytree(os.path.join(CODETOANALYZE_DIR, 'utf8_in_pwd'),
                        utf8_in_pwd_path)

        self.test_cmake_integration(
            enabled=True,
            root=os.path.join(utf8_in_pwd_path, 'cmake'),
            report_fname='utf8_in_pwd_cmake_report.json')
        self.test_gradle_integration(
            enabled=True,
            root=os.path.join(utf8_in_pwd_path, 'gradle'),
            report_fname='utf8_in_pwd_gradle_report.json')
        self.test_javac_integration(
            enabled=True,
            root=os.path.join(utf8_in_pwd_path),
            report_fname='utf8_in_pwd_javac_report.json')
        self.test_make_integration(
            enabled=True,
            root=os.path.join(utf8_in_pwd_path, 'make'),
            report_fname='utf8_in_pwd_make_report.json')
        shutil.rmtree(utf8_in_pwd_path, True) # remove copied dir

    def test_unknown_extension(self):
        test('unknown_ext', 'unknown extension',
             CODETOANALYZE_DIR,
             [{'compile': ['clang', '-x', 'c', '-c', 'hello.unknown_ext']}])

    def test_clang_multiple_source_files(self):
        test('multiclang', 'clang multiple source files',
             CODETOANALYZE_DIR,
             [{'compile': ['clang', '-c', 'hello.c', 'hello2.c']}])

    def test_reactive_multiple_capture(self):
        reactive_args = ['-a', 'capture', '--reactive', '--continue']
        test('reactive', 'reactive with multiple capture',
             CODETOANALYZE_DIR,
             [{'compile': ['clang', '-c', 'hello.c'],
               'infer_args': reactive_args},
              {'compile': ['clang', '-c', 'hello2.c'],
               'infer_args': reactive_args},
              {'compile': ['analyze']}])

    def test_clang_component_kit_analytics(self):
        test('componentkit_analytics',
             'component quality analyzer emits analytics info when flag is '
             'enabled',
             os.path.join(CODETOANALYZE_DIR, 'componentkit'),
             [{'compile': ['clang', '-x', 'objective-c++', '-std=c++11', '-c',
                           '-fblocks', 'TestComponentKitAnalytics.mm'],
               'infer_args': ['--cxx', '--no-filtering', '-a', 'linters',
                              '--compute-analytics']}])

    def test_clang_component_kit_imports(self):
        test('componentkit_imports',
             'component quality analyzer skips imports',
             os.path.join(CODETOANALYZE_DIR, 'componentkit'),
             [{'compile': ['clang', '-x', 'objective-c++', '-std=c++11', '-c',
                           '-fblocks', 'TestIgnoreImports.mm'],
               'infer_args': ['--cxx', '--no-filtering', '-a', 'linters']}])

    def test_fail_on_issue(self):
        test('fail', '--fail-on-issue flag',
             CODETOANALYZE_DIR,
             [{'compile': ['clang', '-c', 'hello.c'],
               'infer_args': ['--fail-on-issue']}],
             should_fail=2)

    def test_pmd_xml_output(self):
        def pmd_check(infer_out):
            assert os.path.exists(os.path.join(infer_out, 'report.xml'))
        try:
            from lxml import etree
            has_lxml = True
        except ImportError:
            has_lxml = False

        test('pmd-xml', 'PMD XML output',
             CODETOANALYZE_DIR,
             [{'compile': ['clang', '-c', 'hello.c'],
               'infer_args': ['--pmd-xml']}],
             extra_check=pmd_check,
             available=lambda: has_lxml)

    def test_infer_deletes_infer_out(self):
        # Test that two consecutive analyses do not pollute each other:
        # the expected results of running infer on hello.c then on
        # hello2.c is that only the bug in hello2.c is reported.
        test('delete', 'infer deletes infer-out',
             CODETOANALYZE_DIR,
             [{'compile': ['clang', '-c', 'hello.c']},
              {'compile': ['clang', '-c', 'hello2.c']}])


if __name__ == '__main__':
    # hackish capturing of the arguments after '--'
    try:
        i = sys.argv.index('--')
        to_test = sys.argv[i + 1:]
        sys.argv = sys.argv[:i]
    except ValueError:
        pass

    unittest.main()  # run all the tests
