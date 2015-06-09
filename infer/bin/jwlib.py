# Copyright (c) 2009-2013 Monoidics ltd.
# Copyright (c) 2013- Facebook.
# All rights reserved.

import argparse
import logging
import tempfile
import os
import subprocess

# javac options
parser = argparse.ArgumentParser()

current_directory = os.getcwd()

parser.add_argument('-version', action='store_true')
parser.add_argument('-cp', '-classpath', type=str, dest='classpath')
parser.add_argument('-bootclasspath', type=str)
parser.add_argument('-d', dest='classes_out')


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

            with tempfile.NamedTemporaryFile(
                    mode='w',
                    suffix='.out',
                    prefix='javac_',
                    delete=False) as file_out:
                self.verbose_out = file_out.name

                try:
                    subprocess.check_call(javac_cmd, stderr=file_out)
                    return os.EX_OK
                except subprocess.CalledProcessError as exc:
                    error_msg = 'Javac compilation error with: \n\n{}\n'
                    failing_cmd = [arg for arg in javac_cmd
                                   if arg != '-verbose']
                    logging.error(error_msg.format(failing_cmd))
                    os.system(' '.join(failing_cmd))
                    return exc.returncode
