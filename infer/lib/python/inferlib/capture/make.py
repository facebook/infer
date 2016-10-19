# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

import logging
import os
import subprocess
import traceback

import util
from inferlib import config, utils

MODULE_NAME = 'make/cc/clang/gcc'
MODULE_DESCRIPTION = '''Run analysis of code built with commands like:
make [target]
clang [compiler_options] <filename>
gcc [compiler_options] <filename>
cc [compiler_options] <filename>

Analysis examples:
infer -- make all
infer -- clang -c srcfile.m
infer -- gcc -c srcfile.c'''
LANG = ['clang']

ALIASED_COMMANDS = ['clang', 'clang++', 'cc', 'gcc', 'g++']
BUILD_COMMANDS = ['cmake', 'configure', 'make', 'waf']
SUPPORTED_COMMANDS = ALIASED_COMMANDS + BUILD_COMMANDS


def gen_instance(*args):
    return MakeCapture(*args)


# This creates an empty argparser for the module, which provides only
# description/usage information and no arguments.
create_argparser = util.base_argparser(MODULE_DESCRIPTION, MODULE_NAME)


class MakeCapture:
    def __init__(self, args, cmd):
        self.args = args
        self.cmd = cmd
        command_name = os.path.basename(cmd[0])
        if command_name in ALIASED_COMMANDS:
            # remove absolute paths for these commands as we want to
            # substitue our own wrappers instead using a PATH trick
            cmd[0] = command_name

    def get_envvars(self):
        env_vars = utils.read_env()
        wrappers_path = config.WRAPPERS_DIRECTORY
        # INFER_RESULTS_DIR and INFER_OLD_PATH are used by javac wrapper only
        env_vars['INFER_OLD_PATH'] = env_vars['PATH']
        env_vars['PATH'] = '{wrappers}{sep}{path}'.format(
            wrappers=wrappers_path,
            sep=os.pathsep,
            path=env_vars['PATH'],
        )
        env_vars['INFER_RESULTS_DIR'] = self.args.infer_out
        return env_vars

    def capture(self):
        try:
            env = utils.encode_env(self.get_envvars())
            cmd = map(utils.encode, self.cmd)
            logging.info('Running command %s with env:\n%s' % (cmd, env))
            subprocess.check_call(cmd, env=env)
            capture_dir = os.path.join(self.args.infer_out, 'captured')
            if len(os.listdir(capture_dir)) < 1:
                # Don't return with a failure code unless we're
                # running make. It could be normal to have captured
                # nothing (eg, empty source file). Further output will
                # alert the user that there was nothing to analyze.
                if self.cmd[0] == 'make':
                    # reuse code from gradle, etc. integration
                    return util.run_compilation_commands([], 'make clean')
            return os.EX_OK
        except subprocess.CalledProcessError as exc:
            if self.args.debug:
                traceback.print_exc()
            return exc.returncode
