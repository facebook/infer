# Copyright (c) 2009 - 2013 Monoidics ltd.
# Copyright (c) 2013 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import argparse
import codecs
import os
import subprocess
import tempfile
import time

from . import analyze, config, utils

# javac options
parser = argparse.ArgumentParser()

current_directory = utils.decode(os.getcwd())

parser.add_argument('-d', dest='classes_out', default=current_directory)


def _get_javac_args(args):
    try:
        javac_pos = args.index('javac')
    except ValueError:
        return None
    javac_args = args[javac_pos + 1:]
    if len(javac_args) == 0:
        return None
    else:
        # replace any -g:.* flag with -g to preserve debugging symbols
        args = map(lambda arg: '-g' if '-g:' in arg else arg, javac_args)
        # skip -Werror
        args = filter(lambda arg: arg != '-Werror', args)
        return args


def create_infer_command(args, javac_arguments):
    infer_args = ['-o', args.infer_out]
    if args.debug:
        infer_args.append('--debug')
    infer_args += ['--analyzer', 'capture']

    return AnalyzerWithJavac(
        analyze.infer_parser.parse_args(infer_args),
        'javac',
        _get_javac_args(['javac'] + javac_arguments)
    )


class CompilerCall(object):

    def __init__(self, javac_cmd, arguments):
        assert javac_cmd is not None and arguments is not None
        self.javac_cmd = javac_cmd
        self.original_arguments = arguments
        self.args, self.remaining_args = parser.parse_known_args(arguments)
        self.verbose_out = None

    def run(self):
        javac_args = ['-verbose', '-g']

        if self.args.classes_out is not None:
            javac_args += ['-d', self.args.classes_out]

        javac_args += self.remaining_args

        def arg_must_go_on_cli(arg):
            # as mandated by javac, argument files must not contain
            # arguments
            return arg.startswith('-J') or arg.startswith('@')
        file_args = filter(lambda x: not arg_must_go_on_cli(x), javac_args)
        cli_args = filter(arg_must_go_on_cli, javac_args)

        # pass non-special args via a file to avoid blowing up the
        # command line size limit
        with tempfile.NamedTemporaryFile(
                mode='w',
                prefix='javac_args_',
                delete=False) as command_line:
            escaped_args = map(lambda x: '"%s"' % (x.replace('"', '\\"')),
                               file_args)
            command_line.write(utils.encode('\n'.join(escaped_args)))
            command_line.write('\n')
            self.command_line_file = command_line.name

        with tempfile.NamedTemporaryFile(
                mode='w',
                suffix='.out',
                prefix='javac_',
                delete=False) as file_out:
            self.verbose_out = file_out.name

            command = self.javac_cmd + cli_args + \
                      ['@' + str(self.command_line_file)]
            try:
                subprocess.check_call(command, stderr=file_out)
            except subprocess.CalledProcessError:
                try:
                    fallback_command = ['javac'] + cli_args + \
                                       ['@' + str(self.command_line_file)]
                    subprocess.check_call(
                        fallback_command, stderr=file_out)
                except subprocess.CalledProcessError:
                    error_msg = 'ERROR: failure during compilation ' \
                                + 'command.\nYou can run the failing ' \
                                + 'compilation command again by ' \
                                + 'copy-pasting the\nlines below in ' \
                                + 'your terminal:\n\n"""\n' \
                                + 'python <<EOF\n' \
                                + 'import subprocess\n' \
                                + 'cmd = {}\n' \
                                + 'subprocess.check_call(cmd)\n' \
                                + 'EOF\n"""\n'
                    failing_cmd = filter(lambda arg: arg != '-verbose',
                                         command)
                    utils.stderr(error_msg.format(failing_cmd))
                    subprocess.check_call(failing_cmd)

        return os.EX_OK


class AnalyzerWithFrontendWrapper(analyze.AnalyzerWrapper):

    def __init__(self, infer_args, compiler_call):
        analyze.AnalyzerWrapper.__init__(self, infer_args)
        self.javac = compiler_call
        if self.javac.original_arguments is None:
            raise Exception('No javac command detected')

    def start(self):
        self._compile()
        if self.args.analyzer == config.ANALYZER_COMPILE:
            return os.EX_OK

        self._run_infer_frontend()
        if self.args.analyzer == config.ANALYZER_CAPTURE:
            return os.EX_OK

        self.analyze_and_report()
        self._close()

        return os.EX_OK

    def _run_infer_frontend(self):
        infer_cmd = [utils.get_cmd_in_bin_dir('InferJava')]

        infer_cmd += [
            '-verbose_out', self.javac.verbose_out,
        ]

        if self.args.debug:
            infer_cmd.append('-debug')
        if self.args.analyzer == config.ANALYZER_TRACING:
            infer_cmd.append('-tracing')
        if self.args.android_harness:
            infer_cmd.append('-harness')

        return analyze.run_command(
            infer_cmd,
            self.args.debug,
            self.javac.original_arguments,
            'frontend',
            self.args.analyzer
        )

    def _compile(self):
        return self.javac.run()

    def _close(self):
        os.remove(self.javac.verbose_out)


class AnalyzerWithJavac(AnalyzerWithFrontendWrapper):

    def __init__(self, infer_args, javac_executable, javac_args):
        javac_cmd = [javac_executable, '-J-Duser.language=en']
        compiler_call = CompilerCall(javac_cmd, javac_args)
        AnalyzerWithFrontendWrapper.__init__(self, infer_args, compiler_call)


class AnalyzerWithJavaJar(AnalyzerWithFrontendWrapper):

    def __init__(self, infer_args, java_executable, jar_path, compiler_args):
        javac_cmd = [java_executable, '-jar', jar_path]
        compiler_call = CompilerCall(javac_cmd, compiler_args)
        AnalyzerWithFrontendWrapper.__init__(self, infer_args, compiler_call)
