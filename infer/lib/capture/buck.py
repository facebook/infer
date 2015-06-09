import os
import subprocess
import traceback
import util

import utils  # this is module located in ../utils.py

MODULE_NAME = __name__
MODULE_DESCRIPTION = '''Run analysis of code built with a command like:
buck [options] [target]

Analysis examples:
infer -- buck build HelloWorld'''


def gen_instance(*args):
    return BuckAnalyzer(*args)

# This creates an empty argparser for the module, which provides only
# description/usage information and no arguments.
create_argparser = util.base_argparser(MODULE_DESCRIPTION, MODULE_NAME)


class BuckAnalyzer:
    def __init__(self, args, cmd):
        self.args = args
        self.cmd = cmd[2:]  # TODO: make the extraction of targets smarter

    def capture(self):
        # BuckAnalyze is a special case, and we run the analysis from here
        capture_cmd = [utils.get_cmd_in_bin_dir('BuckAnalyze')]
        if self.args.debug:
            capture_cmd.append('-g')
        capture_cmd += self.cmd
        capture_cmd += ['--analyzer', self.args.analyzer]
        try:
            subprocess.check_call(capture_cmd)
            return os.EX_OK
        except subprocess.CalledProcessError as exc:
            if self.args.debug:
                traceback.print_exc()
            return exc.returncode
