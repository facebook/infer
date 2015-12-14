# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

import argparse
import json
import logging
import os
import subprocess
import traceback
import util

from inferlib import config, issues, utils

MODULE_NAME = __name__
MODULE_DESCRIPTION = '''Run analysis of code built with a command like:
buck [options] [target]

Analysis examples:
infer -- buck build HelloWorld'''


def gen_instance(*args):
    return BuckAnalyzer(*args)


# This creates an empty argparser for the module, which provides only
# description/usage information and no arguments.
def create_argparser(group_name=MODULE_NAME):
    """This defines the set of arguments that get added by this module to the
    set of global args defined in the infer top-level module
    Do not use this function directly, it should be invoked by the infer
    top-level module"""
    parser = argparse.ArgumentParser(add_help=False)
    group = parser.add_argument_group(
        "{grp} module".format(grp=MODULE_NAME),
        description=MODULE_DESCRIPTION,
    )
    group.add_argument('--verbose', action='store_true',
                       help='Print buck compilation steps')
    group.add_argument('--no-cache', action='store_true',
                       help='Do not use buck distributed cache')
    group.add_argument('--print-harness', action='store_true',
                       help='Print generated harness code (Android only)')
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
            'infer.infer_bin={bin}'
            .format(bin=config.BIN_DIRECTORY),
            '--config',
            'infer.clang_compiler={clang}'.format(clang=clang_path),
            '--config',
            'infer.clang_plugin={plugin}'.format(plugin=plugin_path),
        ]
        if self.args.xcode_developer_dir is not None:
            args.append('--config')
            args.append('apple.xcode_developer_dir={devdir}'.format(
                devdir=self.args.xcode_developer_dir))
        if self.args.blacklist_regex:
            args.append('--config')
            args.append('infer.blacklist_regex={regex}'.format(
                regex=self.args.blacklist_regex))
        return args

    def _get_analysis_result_files(self):
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
        # from the resulting list, get only what ends in json
        return [x for x in out if x.endswith('.json')]

    def _run_buck_with_flavors(self):
        # TODO: Use buck to identify the project's root folder
        if not os.path.isfile('.buckconfig'):
            print('Please run this command from the folder where .buckconfig '
                  'is located')
            return os.EX_USAGE
        subprocess.check_call(
            self.cmd + self.create_cxx_buck_configuration_args())
        return os.EX_OK

    def capture_with_flavors(self):
        ret = self._run_buck_with_flavors()
        if not ret == os.EX_OK:
            return ret
        result_files = self._get_analysis_result_files()
        all_results = issues.merge_reports_from_paths(result_files)
        merged_results_path = os.path.join(self.args.infer_out,
                                           config.JSON_REPORT_FILENAME)
        utils.dump_json_to_path(all_results, merged_results_path)
        print('Results saved in {results_path}'.format(
            results_path=merged_results_path))
        return os.EX_OK

    def capture_without_flavors(self):
        # BuckAnalyze is a special case, and we run the analysis from here
        capture_cmd = [utils.get_cmd_in_bin_dir('BuckAnalyze')]
        if self.args.infer_out is not None:
            capture_cmd += ['--out', self.args.infer_out]
        if self.args.debug:
            capture_cmd.append('-g')
        if self.args.debug_exceptions:
            capture_cmd.append('--debug-exceptions')
        if self.args.no_filtering:
            capture_cmd.append('--no-filtering')
        if self.args.verbose:
            capture_cmd.append('--verbose')
        if self.args.no_cache:
            capture_cmd.append('--no-cache')
        if self.args.print_harness:
            capture_cmd.append('--print-harness')
        capture_cmd += self.cmd[2:]  # TODO: make extraction of targets smarter
        capture_cmd += ['--analyzer', self.args.analyzer]
        subprocess.check_call(capture_cmd)
        return os.EX_OK
