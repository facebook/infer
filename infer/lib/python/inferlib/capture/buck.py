# Copyright (c) 2015 - present Facebook, Inc.
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
import logging
import os
import subprocess
import traceback

from inferlib import config, issues, utils, bucklib
from . import util

MODULE_NAME = __name__
MODULE_DESCRIPTION = '''Run analysis of code built with a command like:
buck [options] [target]

Analysis examples:
infer -- buck build HelloWorld'''
LANG = ['clang', 'java']


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

    return parser


class BuckAnalyzer:
    def __init__(self, args, cmd):
        self.args = args
        self.cmd = cmd
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
            '*//infer.infer_bin={bin}'
            .format(bin=config.BIN_DIRECTORY),
            '--config',
            '*//infer.clang_compiler={clang}'.format(clang=clang_path),
            '--config',
            '*//infer.clang_plugin={plugin}'.format(plugin=plugin_path),
            '--config',
            '*//cxx.pch_enabled=false',
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
        proc = subprocess.Popen(buck_results_cmd, stdout=subprocess.PIPE)
        (buck_output, _) = proc.communicate()
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

    def _run_buck_with_flavors(self):
        # TODO: Use buck to identify the project's root folder
        if not os.path.isfile('.buckconfig'):
            print('Please run this command from the folder where .buckconfig '
                  'is located')
            return os.EX_USAGE
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
        subprocess.check_call(command, env=env)
        return os.EX_OK

    def capture_with_flavors(self):
        ret = self._run_buck_with_flavors()
        if not ret == os.EX_OK:
            return ret
        result_paths = self._get_analysis_result_paths()
        merged_reports_path = os.path.join(
            self.args.infer_out, config.JSON_REPORT_FILENAME)
        merged_deps_path = os.path.join(
            self.args.infer_out, config.INFER_BUCK_DEPS_FILENAME)
        self._merge_infer_report_files(result_paths, merged_reports_path)
        self._merge_infer_dep_files(result_paths, merged_deps_path)
        infer_out = self.args.infer_out
        json_report = os.path.join(infer_out, config.JSON_REPORT_FILENAME)
        bugs_out = os.path.join(infer_out, config.BUGS_FILENAME)
        issues.print_and_save_errors(infer_out, self.args.project_root,
                                     json_report, bugs_out, self.args.pmd_xml)
        return os.EX_OK

    def capture_without_flavors(self):
        # Java is a special case, and we run the analysis from here
        buck_wrapper = bucklib.Wrapper(self.args, self.cmd)
        return buck_wrapper.run()
