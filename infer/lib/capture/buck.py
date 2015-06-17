import argparse
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
    return parser



class BuckAnalyzer:
    def __init__(self, args, cmd):
        self.args = args
        self.cmd = cmd[2:]  # TODO: make the extraction of targets smarter

    def capture(self):
        # BuckAnalyze is a special case, and we run the analysis from here
        capture_cmd = [utils.get_cmd_in_bin_dir('BuckAnalyze')]
        if self.args.debug:
            capture_cmd.append('-g')
        if self.args.no_filtering:
            capture_cmd.append('--no-filtering')
        if self.args.verbose:
            capture_cmd.append('--verbose')
        if self.args.no_cache:
            capture_cmd.append('--no-cache')
        if self.args.print_harness:
            capture_cmd.append('--print-harness')

        capture_cmd += self.cmd
        capture_cmd += ['--analyzer', self.args.analyzer]
        try:
            subprocess.check_call(capture_cmd)
            return os.EX_OK
        except subprocess.CalledProcessError as exc:
            if self.args.debug:
                traceback.print_exc()
            return exc.returncode
