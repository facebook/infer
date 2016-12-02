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

parser.add_argument('-version', action='store_true')
parser.add_argument('-cp', '-classpath', type=utils.decode,
                    dest='classpath', default=os.getcwd())
parser.add_argument('-d', dest='classes_out', default=current_directory)
parser.add_argument('-processorpath', type=utils.decode, dest='processorpath')
parser.add_argument('-processor', type=utils.decode, dest='processor')


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


# return True if string is empty or an escaped variant of empty
def _is_empty_classpath(string):
    stripped = string.strip("'").strip('"')
    if stripped == '':
        return True
    elif stripped == string:
        return False
    else:
        return _is_empty_classpath(stripped)


class AnnotationProcessorNotFound(Exception):

    def __init__(self, path):
        self.path = path

    def __str__(self):
        return repr(self.path + ' not found')


class CompilerCall(object):

    def __init__(self, javac_cmd, arguments):
        assert javac_cmd is not None and arguments is not None
        self.javac_cmd = javac_cmd
        self.original_arguments = arguments
        self.args, self.remaining_args = parser.parse_known_args(arguments)
        self.verbose_out = None

    def get_version(self):
        assert self.args.version
        return subprocess.check_output(
            self.javac_cmd + self.original_arguments,
            stderr=subprocess.STDOUT).strip()

    def run(self):
        if self.args.version:
            return subprocess.call(self.javac_cmd + self.original_arguments)
        else:
            javac_args = ['-verbose', '-g']

            if not os.path.isfile(config.ANNOT_PROCESSOR_JAR):
                raise AnnotationProcessorNotFound(config.ANNOT_PROCESSOR_JAR)
            if self.args.classes_out is not None:
                javac_args += ['-d', self.args.classes_out]

            classpath = self.args.classpath
            # the -processorpath option precludes searching the classpath for
            # annotation processors, so we don't want to use it unless the
            # javac command does
            if self.args.processorpath is not None:
                processorpath = os.pathsep.join([config.ANNOT_PROCESSOR_JAR,
                                                 self.args.processorpath])
                javac_args += ['-processorpath', processorpath]
            else:
                classpath = os.pathsep.join([config.ANNOT_PROCESSOR_JAR,
                                             classpath])

            at_args = [
                arg for arg in self.remaining_args if arg.startswith('@')]
            found_classpath = False
            for at_arg in at_args:
                # remove @ character
                args_file_name = at_arg[1:]
                with codecs.open(args_file_name, 'r',
                                 encoding=config.CODESET) as args_file:
                    args_file_contents = args_file.read()
                prefix_suffix = args_file_contents.split('-classpath\n', 1)
                if len(prefix_suffix) == 2:
                    prefix, suffix = prefix_suffix
                    with tempfile.NamedTemporaryFile(
                            dir=self.args.infer_out,
                            suffix='_cp',
                            delete=False) as args_file_cp:
                        cp_line, _, after_cp = suffix.partition('\n')
                        # avoid errors the happen when we get -classpath '"',
                        # -classpath '', or similar from the args file
                        if _is_empty_classpath(cp_line):
                            cp_line = '\n'
                        else:
                            cp_line = ':{}\n'.format(cp_line)
                        cp_line = prefix + '-classpath\n' + classpath + cp_line
                        cp_line += after_cp
                        args_file_cp.write(cp_line)
                        at_arg_cp = '@' + args_file_cp.name
                        self.remaining_args = [
                            at_arg_cp if arg == at_arg else arg
                            for arg in self.remaining_args]
                        found_classpath = True
                        break

            javac_args += self.remaining_args

            if not found_classpath:
                # -classpath option not found in any args file
                javac_args += ['-classpath', classpath]

            # this overrides the default mechanism for discovering annotation
            # processors (checking the manifest of the annotation processor
            # JAR), so we don't want to use it unless the javac command does
            if self.args.processor is not None:
                processor = '{infer_processors},{original_processors}'.format(
                    infer_processors=config.ANNOT_PROCESSOR_NAMES,
                    original_processors=self.args.processor
                )
                javac_args += ['-processor', processor]

            with tempfile.NamedTemporaryFile(
                    mode='w',
                    suffix='.out',
                    prefix='annotations_',
                    delete=False) as annot_out:
                # Initialize the contents of the file to a valid empty
                # json object.
                annot_out.write('{}')
                self.suppress_warnings_out = annot_out.name
            javac_args += ['-A%s=%s' %
                           (config.SUPRESS_WARNINGS_OUTPUT_FILENAME_OPTION,
                            self.suppress_warnings_out)]

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
        if not self.javac.args.version:
            if self.javac.original_arguments is None:
                raise Exception('No javac command detected')

        if self.args.buck:
            self.args.infer_out = os.path.join(
                self.javac.args.classes_out,
                config.BUCK_INFER_OUT)
            self.args.infer_out = os.path.abspath(self.args.infer_out)

    def compute_buck_key(self):
        javac_version = self.javac.get_version()
        infer_version = utils.infer_key(self.args.analyzer)
        return '/'.join([javac_version, infer_version])

    def start(self):
        if self.javac.args.version:
            if self.args.buck:
                utils.stderr(self.compute_buck_key())
            else:
                return self.javac.run()
        else:
            start_time = time.time()

            self._compile()
            if self.args.analyzer == config.ANALYZER_COMPILE:
                return os.EX_OK

            self._run_infer_frontend()
            self.timing['capture'] = utils.elapsed_time(start_time)
            if self.args.analyzer == config.ANALYZER_CAPTURE:
                return os.EX_OK

            if self.args.buck:
                if self.javac.args.classpath is not None:
                    infer_args = utils.read_env()['INFER_ARGS']
                    # 'INFER_ARGS' must be CommandLineOption.args_env_var
                    for path in self.javac.args.classpath.split(os.pathsep):
                        if os.path.isfile(path):
                            infer_args += '^--specs-library^' + os.path.abspath(path)
                            # '^' must be CommandLineOption.env_var_sep
                    os.environ['INFER_ARGS'] = utils.encode(infer_args)

            self.analyze_and_report()
            self._close()
            self.timing['total'] = utils.elapsed_time(start_time)
            self.save_stats()

            return self.stats

    def _run_infer_frontend(self):
        infer_cmd = [utils.get_cmd_in_bin_dir('InferJava')]

        infer_cmd += [
            '-results_dir', self.args.infer_out,
            '-verbose_out', self.javac.verbose_out,
            '-suppress_warnings_out', self.javac.suppress_warnings_out,
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
        os.remove(self.javac.suppress_warnings_out)


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
