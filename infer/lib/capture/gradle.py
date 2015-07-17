# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

import os
import util
import logging
import subprocess
import tempfile

MODULE_NAME = __name__
MODULE_DESCRIPTION = '''Run analysis of code built with a command like:
gradle [options] [task]

Analysis examples:
infer -- gradle build
infer -- ./gradlew build'''

FILELISTS = 'filelists'

def gen_instance(*args):
    return GradleCapture(*args)

# This creates an empty argparser for the module, which provides only
# description/usage information and no arguments.
create_argparser = util.base_argparser(MODULE_DESCRIPTION, MODULE_NAME)


class GradleCapture:

    def __init__(self, args, cmd):
        self.args = args
        # TODO: make the extraction of targets smarter
        self.build_cmd = [cmd[0], '--debug'] + cmd[1:]
        # That contains javac version as well
        version_str = util.run_cmd_ignore_fail([cmd[0], '--version'])
        path = os.path.join(self.args.infer_out, FILELISTS)
        if not os.path.exists(path):
            os.mkdir(path)
        logging.info("Running with:\n" + version_str)

    def get_inferJ_commands(self, verbose_output):
        argument_start_pattern = ' Compiler arguments: '
        calls = []
        for line in verbose_output:
            if argument_start_pattern in line:
                content = line.partition(argument_start_pattern)[2].strip()
                javac_arguments = content.split(' ')
                java_files = []
                java_args = []
                for java_arg in javac_arguments:
                    if java_arg.endswith('.java'):
                        java_files.append(java_arg)
                    else:
                        java_args.append(java_arg)
                with tempfile.NamedTemporaryFile(
                        mode='w',
                        suffix='.txt',
                        prefix='gradle_',
                        dir=os.path.join(self.args.infer_out, FILELISTS),
                        delete=False) as sources:
                    sources.write("\n".join(java_files))
                    sources.flush()
                    java_args.append("@" + sources.name)
                    capture = util.create_inferJ_command(self.args,
                                                         java_args)
                    calls.append(capture)
        return calls

    def capture(self):
        cmds = self.get_inferJ_commands(util.get_build_output(self.build_cmd))
        return util.run_commands(cmds)
