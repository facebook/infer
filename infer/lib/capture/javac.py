import os
import subprocess
import traceback
import util

import inferlib

MODULE_NAME = __name__
MODULE_DESCRIPTION = '''Run analysis of code built with a command like:
javac <options> <source files>

Analysis examples:
infer -- javac srcfile.java'''


def gen_instance(*args):
    return JavacCapture(*args)

# This creates an empty argparser for the module, which provides only
# description/usage information and no arguments.
create_argparser = util.base_argparser(MODULE_DESCRIPTION, MODULE_NAME)


class JavacCapture:
    def __init__(self, args, cmd):
        self.analysis = inferlib.Infer(args, cmd[1:])

    def capture(self):
        try:
            self.analysis.start()
            return os.EX_OK
        except subprocess.CalledProcessError as exc:
            if self.analysis.args.debug:
                traceback.print_exc()
            return exc.returncode
