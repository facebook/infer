import os
import subprocess
import traceback
import util

import utils  # this is module located in ../utils.py
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
        self.args = args
        self.cmd = cmd

    def capture(self):
        # run inferJ only in capture mode
        # pass all the frontend args (if any)
        capture_cmd = [utils.get_cmd_in_bin_dir('inferJ')]
        capture_cmd += ['--out', self.args.infer_out]
        capture_cmd += ['--analyzer', self.args.analyzer]
        if self.args.debug:
            capture_cmd.append('-g')
        capture_cmd += self.cmd

        try:
            subprocess.check_call(capture_cmd)
            return os.EX_OK
        except subprocess.CalledProcessError as exc:
            if self.args.debug:
                traceback.print_exc()
            return exc.returncode
