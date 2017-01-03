# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

import os
import logging
import re
import util

from inferlib import jwlib

MODULE_NAME = __name__
MODULE_DESCRIPTION = '''Run analysis of code built with a command like:
mvn [options] [task]

Analysis examples:
infer -- mvn build'''
LANG = ['java']


def gen_instance(*args):
    return MavenCapture(*args)

# This creates an empty argparser for the module, which provides only
# description/usage information and no arguments.
create_argparser = util.base_argparser(MODULE_DESCRIPTION, MODULE_NAME)


class MavenCapture:
    def __init__(self, args, cmd):
        self.args = args
        logging.info(util.run_cmd_ignore_fail(['mvn', '-version']))
        # TODO: make the extraction of targets smarter
        self.build_cmd = ['mvn', '-X'] + cmd[1:]

    def get_infer_commands(self, verbose_output):
        file_pattern = r'\[DEBUG\] Stale source detected: ([^ ]*\.java)'
        options_pattern = '[DEBUG] Command line options:'
        source_roots_pattern = '[DEBUG] Source roots:'

        files_to_compile = []
        calls = []
        options_next = False
        source_roots_next = False
        for line in verbose_output:
            if options_next:
                #  line has format [Debug] <space separated options>
                javac_args = line.split(' ')[1:] + files_to_compile
                capture = jwlib.create_infer_command(javac_args)
                calls.append(capture)
                options_next = False
                files_to_compile = []

            elif source_roots_next:
                # line has format [Debug] <space separated directories>
                src_roots = line.split(' ')[1:]
                for src_root in src_roots:
                    for root, dirs, files in os.walk(src_root):
                        for name in files:
                            if name.endswith(".java"):
                                path = os.path.join(root, name)
                                files_to_compile.append(path)
                source_roots_next = False

            elif options_pattern in line:
                #  Next line will have javac options to run
                options_next = True

            elif source_roots_pattern in line:
                # Next line will have directory containing files to compile
                source_roots_next = True

            else:
                found = re.match(file_pattern, line)
                if found:
                    files_to_compile.append(found.group(1))

        return calls

    def capture(self):
        cmds = self.get_infer_commands(util.get_build_output(self.build_cmd))
        clean_cmd = '%s clean' % self.build_cmd[0]
        return util.run_compilation_commands(cmds, clean_cmd)
