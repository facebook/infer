import os
import util
import logging
import subprocess

MODULE_NAME = __name__
MODULE_DESCRIPTION = '''Run analysis of code built with a command like:
gradle [options] [task]

Analysis examples:
infer -- gradle build
infer -- ./gradlew build'''


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
        logging.info("Running with:\n" + version_str)

    def get_inferJ_commands(self, verbose_output):
        argument_start_pattern = ' Compiler arguments: '
        calls = []
        for line in verbose_output:
            if argument_start_pattern in line:
                content = line.partition(argument_start_pattern)[2].strip()
                javac_arguments = content.split(' ')
                capture = util.create_inferJ_command(self.args,
                                                     javac_arguments)
                calls.append(capture)
        return calls

    def capture(self):
        cmds = self.get_inferJ_commands(util.get_build_output(self.build_cmd))
        return util.run_commands(cmds)
