# Copyright (c) 2009 - 2013 Monoidics ltd.
# Copyright (c) 2013 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

import argparse
import os
import tempfile
import subprocess
import utils

FILELISTS = 'filelists'

# javac options
parser = argparse.ArgumentParser()

current_directory = os.getcwd()

parser.add_argument('-version', action='store_true')
parser.add_argument('-cp', '-classpath', type=str, dest='classpath')
parser.add_argument('-bootclasspath', type=str)
parser.add_argument('-d', dest='classes_out', default=current_directory)


class CompilerCall:

    def __init__(self, arguments):

        self.original_arguments = arguments
        self.args, _ = parser.parse_known_args(arguments)
        self.verbose_out = None

    def run(self):
        if self.args.version:
            return subprocess.call(['javac'] + self.original_arguments)
        else:
            javac_cmd = ['javac', '-verbose', '-g'] + self.original_arguments
            javac_cmd.append('-J-Duser.language=en')

            with tempfile.NamedTemporaryFile(
                    mode='w',
                    suffix='.out',
                    prefix='javac_',
                    delete=False) as file_out:
                self.verbose_out = file_out.name
                try:
                    subprocess.check_call(javac_cmd, stderr=file_out)
                except subprocess.CalledProcessError:
                    error_msg = 'Javac compilation error with: \n\n{}\n'
                    failing_cmd = [arg for arg in javac_cmd
                                   if arg != '-verbose']
                    utils.error(error_msg.format(failing_cmd))
                    subprocess.check_call(failing_cmd)

        return os.EX_OK
