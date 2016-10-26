# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

import argparse
import os
import subprocess
import traceback

import util
from inferlib import jwlib, utils

MODULE_NAME = __name__
MODULE_DESCRIPTION = '''Run analysis of code built with a command like:
java -jar compiler.jar <options> <source files>

Analysis examples:
infer -- java -jar compiler.jar srcfile.java
infer -- /path/to/java -jar compiler.jar srcfile.java'''
LANG = ['java']


def gen_instance(*args):
    return JavaJarCapture(*args)

# This creates an empty argparser for the module, which provides only
# description/usage information and no arguments.
create_argparser = util.base_argparser(MODULE_DESCRIPTION, MODULE_NAME)


def parse_command_line(cmd):
    cmd_parser = argparse.ArgumentParser()
    cmd_parser.add_argument('-jar', type=utils.decode, metavar='Compiler jar')
    java_jar, other_args = cmd_parser.parse_known_args(cmd[1:])
    if java_jar.jar is None:
        utils.stderr('Expects a javac command or jar file for the compiler')
        utils.stderr('Example: infer -- java -jar compiler.jar ...\n')
        exit(1)
    return cmd[0], java_jar.jar, other_args


class JavaJarCapture:
    def __init__(self, args, cmd):
        java_binary, java_jar, other_args = parse_command_line(cmd)
        if args.java_jar_compiler is not None:
            java_jar = args.java_jar_compiler
        self.analysis = jwlib.AnalyzerWithJavaJar(
            args,
            java_binary,
            java_jar,
            other_args)

    def capture(self):
        try:
            self.analysis.start()
            return os.EX_OK
        except subprocess.CalledProcessError as exc:
            if self.analysis.args.debug:
                traceback.print_exc()
            return exc.returncode
