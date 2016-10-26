import util

# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

from inferlib import config, utils
import subprocess

MODULE_NAME = 'clang-compilation-database'
MODULE_DESCRIPTION = '''Run analysis of code built with the compilation database proided:
clang-compilation-database db.json'''

# This creates an empty argparser for the module, which provides only
# description/usage information and no arguments.
create_argparser = util.base_argparser(MODULE_DESCRIPTION, MODULE_NAME)


def gen_instance(*args):
    return ClangCompilationDatabase(*args)


class ClangCompilationDatabase:
    def __init__(self, args, cmd):
        self.args = args
        self.cmd = cmd

    def capture(self):
        args = self.cmd
        cmd = [utils.get_cmd_in_bin_dir('InferBuckCompilationDatabase')]
        if self.args.project_root:
            cmd += ['--project-root', self.args.project_root]
        cmd += ['--clang-compilation-database', args[1]]
        print(cmd)
        return subprocess.check_call(cmd)
