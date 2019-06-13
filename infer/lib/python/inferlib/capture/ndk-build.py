# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import os
import subprocess
import traceback

import util
from inferlib import config, utils

MODULE_NAME = 'ndk-build/clang'
MODULE_DESCRIPTION = '''Run analysis of code built with ndk-build

    Analysis examples:
    infer -- ndk-build'''
LANG = ['clang']

ALIASED_COMMANDS = ['clang', 'clang++', 'cc', 'gcc', 'g++']

def gen_instance(*args):
    return NdkBuildCapture(*args)


create_argparser = util.base_argparser(MODULE_DESCRIPTION, MODULE_NAME)


class NdkBuildCapture():
    def __init__(self, args, cmd):
        cmd = [
            cmd[0],
            'NDK_TOOLCHAIN_VERSION=clang',
            'TARGET_CC=clang',
            'TARGET_CXX=clang',
            'TARGET_LD=ld',
        ] + cmd[1:]
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
            return os.EX_OK
        except subprocess.CalledProcessError as exc:
            if self.args.debug:
                traceback.print_exc()
            return exc.returncode
