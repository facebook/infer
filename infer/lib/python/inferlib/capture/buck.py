# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import argparse
import logging
import os
import shutil
import subprocess
import tempfile
import traceback
import time

from inferlib import config, issues, utils, bucklib
from . import util

import re

MODULE_NAME = __name__
MODULE_DESCRIPTION = '''Run analysis of code built with a command like:
buck [options] [target]

Analysis examples:
infer -- buck build HelloWorld'''
LANG = ['clang', 'java']

KEEP_GOING_OPTION = "--keep-going"


def gen_instance(*args):
    return BuckAnalyzer(*args)


def string_in_quotes(value):
    return value.strip('\'')


def create_argparser(group_name=MODULE_NAME):
    """This defines the set of arguments that get added by this module to the
    set of global args defined in the infer top-level module
    Do not use this function directly, it should be invoked by the infer
    top-level module"""
    parser = argparse.ArgumentParser(add_help=False)
    group = parser.add_argument_group(
        '{grp} module'.format(grp=MODULE_NAME),
        description=MODULE_DESCRIPTION,
    )
    group.add_argument('--use-flavors', action='store_true',
                       help='Run Infer analysis through the use of flavors. '
                            'Currently this is supported only for the cxx_* '
                            'targets of Buck - e.g. cxx_library, cxx_binary - '
                            'and not for Java. Note: this flag should be used '
                            'in combination with passing the #infer flavor '
                            'to the Buck target.')
    group.add_argument('--xcode-developer-dir',
                       help='Specify the path to Xcode developer directory '
                            '(requires --use-flavors to work)')
    group.add_argument('--blacklist-regex',
                       help='Specify the regex for files to skip during '
                            'the analysis (requires --use-flavors to work)')
    group.add_argument('--Xbuck', action='append', default=[],
                       type=string_in_quotes,
                       help='Pass values as command-line arguments to '
                            'invocations of `buck build`.'
                            'NOTE: value should be wrapped in single quotes')
    group.add_argument('--buck-merge-all-deps',
                       action='store_true',
                       help='Find and merge all deps produced by buck')

    return parser


class BuckAnalyzer:
    def __init__(self, args, cmd):
        self.args = args
        self.cmd = cmd
        self.keep_going = KEEP_GOING_OPTION in self.args.Xbuck
        util.log_java_version()
        logging.info(util.run_cmd_ignore_fail(['buck', '--version']))

    def capture(self):
        try:
            if self.args.use_flavors:
                return self.capture_with_flavors()
            else:
                return self.capture_without_flavors()
        except subprocess.CalledProcessError as exc:
            if self.args.debug:
                traceback.print_exc()
            return exc.returncode

    def create_cxx_buck_configuration_args(self):
        # return a string that can be passed in input to buck
        # and configures the paths to infer/clang/plugin/xcode
        facebook_clang_plugins_root = config.FCP_DIRECTORY
        clang_path = os.path.join(
            facebook_clang_plugins_root,
            'clang',
            'install',
            'bin',
            'clang',
        )
        plugin_path = os.path.join(
            facebook_clang_plugins_root,
            'libtooling',
            'build',
            'FacebookClangPlugin.dylib',
        )
        args = [
            '--config',
            'client.id=infer.clang',
            '--config',
            '*//infer.infer_bin={bin}'
            .format(bin=config.BIN_DIRECTORY),
            '--config',
            '*//infer.clang_compiler={clang}'.format(clang=clang_path),
            '--config',
            '*//infer.clang_plugin={plugin}'.format(plugin=plugin_path),
            '--config',
            '*//cxx.pch_enabled=false',
            '--config',  # Infer doesn't support C++ modules yet (T35656509)
            '*//cxx.modules_default=false',
            '--config',
            '*//cxx.modules=False',
        ] + self.args.Xbuck

        if self.args.xcode_developer_dir is not None:
            args.append('--config')
            args.append('apple.xcode_developer_dir={devdir}'.format(
                devdir=self.args.xcode_developer_dir))
        if self.args.blacklist_regex:
            args.append('--config')
            args.append('*//infer.blacklist_regex={regex}'.format(
                regex=self.args.blacklist_regex))
        return args

    def _get_analysis_result_paths(self):
        # TODO(8610738): Make targets extraction smarter
        buck_results_cmd = [
            self.cmd[0],
            'targets',
            '--show-output'
        ] + self.cmd[2:] + self.create_cxx_buck_configuration_args()
        buck_results_cmd = \
            [x for x in buck_results_cmd if x != KEEP_GOING_OPTION]
        proc = subprocess.Popen(buck_results_cmd, stdout=subprocess.PIPE)
        (buck_output, _) = proc.communicate()
        if proc.returncode != 0:
            return None
        # remove target name prefixes from each line and split them into a list
        out = [x.split(None, 1)[1] for x in buck_output.strip().split('\n')]
        return [os.path.dirname(x)
                if os.path.isfile(x) else x
                for x in out if os.path.exists(x)]

    @staticmethod
    def _merge_infer_dep_files(root_paths, merged_out_path):
        potential_dep_files = [os.path.join(p, config.INFER_BUCK_DEPS_FILENAME)
                               for p in root_paths]
        dep_files = filter(os.path.exists, potential_dep_files)
        utils.merge_and_dedup_files_into_path(dep_files, merged_out_path)

    @staticmethod
    def _merge_infer_report_files(root_paths, merged_out_path):
        potential_report_files = [os.path.join(p, config.JSON_REPORT_FILENAME)
                                  for p in root_paths]
        report_files = filter(os.path.exists, potential_report_files)
        all_results = issues.merge_reports_from_paths(report_files)
        utils.dump_json_to_path(all_results, merged_out_path)

    @staticmethod
    def _find_deps_and_merge(merged_out_path):
        """This function is used to compute the infer-deps.txt file that
        contains the location of the infer-out folders with the captured
        files created by buck. This is needed when keep-going is passed
        to buck and there are compilation failures, because in that case
        buck doesn't create this file."""
        infer_out_folders = []
        start_time = time.time()
        print('finding captured files in buck-out...')
        for root, dirs, files in os.walk(config.BUCK_OUT_GEN):
            regex = re.compile('.*infer-out.*')
            folders = \
                [os.path.join(root, d) for d in dirs if re.match(regex, d)]
            for d in folders:
                if d not in infer_out_folders:
                    infer_out_folders.append(d)
        with open(merged_out_path, 'w') as fmerged_out_path:
            for dir in infer_out_folders:
                fmerged_out_path.write('\t' + '\t' + dir + '\n')
        elapsed_time = time.time() - start_time
        print('time elapsed in finding captured files in buck-out: % 6.2fs'
              % elapsed_time)

    def _find_depsfiles_and_merge(self, merge_out_path):
        """ Sometimes buck targets --show-output gets confused and returns a
        folder that doesn't contain infer-deps.txt. This can happen with on
        for example objc targes with a certain combination of BUCK modes and
        flavours. This function will walk buck-out and find infer-deps.txt
        It will merge ALL infer-deps.txt in buck-out, so you might want
        to do a buck clean first."""
        fs = []
        for root, dirs, files in os.walk(config.BUCK_OUT_GEN):
            fs += [os.path.dirname(os.path.join(root, f)) for f in files
                   if f == config.INFER_BUCK_DEPS_FILENAME]
        self._merge_infer_dep_files(fs, merge_out_path)

    def _move_buck_out(self):
        """ If keep-going is passed, we may need to compute the infer-deps
        file with the paths to the captured files. To make sure that
        this is done in a consistent way, we need to start the analysis
        with an empty buck-out folder."""
        if not os.path.exists(config.BUCK_OUT_TRASH):
            os.makedirs(config.BUCK_OUT_TRASH)
        tmp = tempfile.mkdtemp(
              dir=config.BUCK_OUT_TRASH,
              prefix=config.BUCK_OUT)
        print('moving files in ' + config.BUCK_OUT + ' to ' + tmp)
        for filename in os.listdir(config.BUCK_OUT):
            if filename != config.TRASH:
                shutil.move(os.path.join(config.BUCK_OUT, filename), tmp)

    def _run_buck_with_flavors(self):
        env_vars = utils.read_env()
        infer_args = env_vars['INFER_ARGS']
        if infer_args != '':
            infer_args += '^'  # '^' must be CommandLineOption.env_var_sep
        infer_args += '--fcp-syntax-only'
        env_vars['INFER_ARGS'] = infer_args
        env = utils.encode_env(env_vars)
        command = self.cmd
        command += ['-j', str(self.args.multicore)]
        if self.args.load_average is not None:
            command += ['-L', str(self.args.load_average)]
        command += self.create_cxx_buck_configuration_args()
        try:
            subprocess.check_call(command, env=env)
            return os.EX_OK
        except subprocess.CalledProcessError as e:
            if self.keep_going:
                print('Buck failed, but continuing the analysis '
                      'because --keep-going was passed')
                return -1
            else:
                raise e

    def capture_with_flavors(self):
        if self.keep_going and not self.args.continue_capture:
            self._move_buck_out()
        ret = self._run_buck_with_flavors()
        if not ret == os.EX_OK and not self.keep_going:
            return ret
        result_paths = self._get_analysis_result_paths()
        if result_paths is None:
            # huho, the Buck command to extract results paths failed
            return os.EX_SOFTWARE
        merged_reports_path = os.path.join(
            self.args.infer_out, config.JSON_REPORT_FILENAME)
        merged_deps_path = os.path.join(
            self.args.infer_out, config.INFER_BUCK_DEPS_FILENAME)
        self._merge_infer_report_files(result_paths, merged_reports_path)
        if (not ret == os.EX_OK and self.keep_going):
            self._find_deps_and_merge(merged_deps_path)
        elif self.args.buck_merge_all_deps:
            self._find_depsfiles_and_merge(merged_deps_path)
        else:
            self._merge_infer_dep_files(result_paths, merged_deps_path)
        infer_out = self.args.infer_out
        json_report = os.path.join(infer_out, config.JSON_REPORT_FILENAME)
        bugs_out = os.path.join(infer_out, config.BUGS_FILENAME)
        issues.print_and_save_errors(infer_out, self.args.project_root,
                                     json_report, bugs_out, self.args.pmd_xml,
                                     console_out=not self.args.quiet)
        return os.EX_OK

    def capture_without_flavors(self):
        # Java is a special case, and we run the analysis from here
        buck_wrapper = bucklib.Wrapper(self.args, self.cmd)
        return buck_wrapper.run()
