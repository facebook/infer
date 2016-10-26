# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

import os
import subprocess
import traceback

import util
from inferlib import jwlib

MODULE_NAME = __name__
MODULE_DESCRIPTION = '''Run analysis of code built with a command like:
javac <options> <source files>

Analysis examples:
infer -- javac srcfile.java
infer -- /path/to/javac srcfile.java'''
LANG = ['java']


def gen_instance(*args):
    return JavacCapture(*args)

# This creates an empty argparser for the module, which provides only
# description/usage information and no arguments.
create_argparser = util.base_argparser(MODULE_DESCRIPTION, MODULE_NAME)


class JavacCapture:
    def __init__(self, args, cmd):
        if args.java_jar_compiler is not None:
            self.analysis = jwlib.AnalyzerWithJavaJar(
                args,
                'java',
                args.java_jar_compiler,
                cmd[1:])
        else:
            self.analysis = jwlib.AnalyzerWithJavac(
                args,
                cmd[0],
                cmd[1:])

    def capture(self):
        try:
            self.analysis.start()
            return os.EX_OK
        except subprocess.CalledProcessError as exc:
            if self.analysis.args.debug:
                traceback.print_exc()
            return exc.returncode
