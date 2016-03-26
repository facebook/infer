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
import os
import subprocess
import tempfile
import time

from . import analyze, config, utils

# javac options
parser = argparse.ArgumentParser()

current_directory = os.getcwd()

parser.add_argument('-version', action='store_true')
parser.add_argument('-deprecation', action='store_true')
parser.add_argument('-cp', '-classpath', type=str,
                    dest='classpath', default=os.getcwd())
parser.add_argument('-bootclasspath', type=str)
parser.add_argument('-d', dest='classes_out', default=current_directory)
parser.add_argument('-processorpath', type=str, dest='processorpath')
parser.add_argument('-processor', type=str, dest='processor')


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

    return AnalyzerWithFrontendWrapper(
        analyze.infer_parser.parse_args(infer_args),
        'javac',
        _get_javac_args(['javac'] + javac_arguments)
    )


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

    def run(self):
        if self.args.version:
            return subprocess.call([self.javac_cmd] + self.original_arguments)
        else:
            javac_cmd = [self.javac_cmd, '-verbose', '-g']

            if self.args.bootclasspath is not None:
                javac_cmd += ['-bootclasspath', self.args.bootclasspath]

            if not os.path.isfile(config.ANNOT_PROCESSOR_JAR):
                raise AnnotationProcessorNotFound(config.ANNOT_PROCESSOR_JAR)
            if self.args.classes_out is not None:
                javac_cmd += ['-d', self.args.classes_out]
            javac_cmd += self.remaining_args

            javac_cmd.append('-J-Duser.language=en')

            classpath = self.args.classpath
            # the -processorpath option precludes searching the classpath for
            # annotation processors, so we don't want to use it unless the
            # javac command does
            if self.args.processorpath is not None:
                processorpath = os.pathsep.join([config.ANNOT_PROCESSOR_JAR,
                                                 self.args.processorpath])
                javac_cmd += ['-processorpath', processorpath]
            else:
                classpath = os.pathsep.join([config.ANNOT_PROCESSOR_JAR,
                                             classpath])

            javac_cmd += ['-classpath', classpath]

            # this overrides the default mechanism for discovering annotation
            # processors (checking the manifest of the annotation processor
            # JAR), so we don't want to use it unless the javac command does
            if self.args.processor is not None:
                processor = '%s,%s' % (config.ANNOT_PROCESSOR_NAMES,
                                       self.args.processor)
                javac_cmd += ['-processor', processor]

            with tempfile.NamedTemporaryFile(
                    mode='w',
                    suffix='.json',
                    prefix='classSourceMap_',
                    delete=False) as class_source_map_out:
                self.class_source_map = class_source_map_out.name
            javac_cmd += ['-A%s=%s' %
                          (config.CLASS_SOURCE_MAP_OUTPUT_FILENAME_OPTION,
                           self.class_source_map)]

            with tempfile.NamedTemporaryFile(
                    mode='w',
                    suffix='.out',
                    prefix='annotations_',
                    delete=False) as annot_out:
                self.suppress_warnings_out = annot_out.name
            javac_cmd += ['-A%s=%s' %
                          (config.SUPRESS_WARNINGS_OUTPUT_FILENAME_OPTION,
                           self.suppress_warnings_out)]

            with tempfile.NamedTemporaryFile(
                    mode='w',
                    suffix='.out',
                    prefix='javac_',
                    delete=False) as file_out:
                self.verbose_out = file_out.name
                try:
                    subprocess.check_call(javac_cmd, stderr=file_out)
                except subprocess.CalledProcessError:
                    error_msg = 'ERROR: failure during compilation command.' \
                                + '\nYou can run the failing compilation ' \
                                + 'command again by copy-pasting the\nlines ' \
                                + 'below in your terminal:\n\n"""\n' \
                                + 'python <<EOF\n' \
                                + 'import subprocess\n' \
                                + 'cmd = {}\n' \
                                + 'subprocess.check_call(cmd)\n' \
                                + 'EOF\n"""\n'
                    failing_cmd = filter(lambda arg: arg != '-verbose',
                                         javac_cmd)
                    utils.stderr(error_msg.format(failing_cmd))
                    subprocess.check_call(failing_cmd)

        return os.EX_OK


class AnalyzerWithFrontendWrapper(analyze.AnalyzerWrapper):

    def __init__(self, args, javac_cmd, javac_args):
        self.javac = CompilerCall(javac_cmd, javac_args)
        if not self.javac.args.version:
            if javac_args is None:
                help_exit('No javac command detected')

        analyze.AnalyzerWrapper.__init__(self, args)

        if self.args.buck:
            self.args.infer_out = os.path.join(
                self.javac.args.classes_out,
                config.BUCK_INFER_OUT)
            self.args.infer_out = os.path.abspath(self.args.infer_out)

    def start(self):
        if self.javac.args.version:
            if self.args.buck:
                key = self.args.analyzer
                utils.stderr(utils.infer_key(key))
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

            self.analyze_and_report()
            self._close()
            self.timing['total'] = utils.elapsed_time(start_time)
            self.save_stats()

            return self.stats

    # create a classpath to pass to the frontend
    def _create_frontend_classpath(self):
        classes_out = '.'
        if self.javac.args.classes_out is not None:
            classes_out = self.javac.args.classes_out
        classes_out = os.path.abspath(classes_out)
        original_classpath = []
        if self.javac.args.bootclasspath is not None:
            original_classpath = self.javac.args.bootclasspath.split(':')
        if self.javac.args.classpath is not None:
            original_classpath += self.javac.args.classpath.split(':')
        if len(original_classpath) > 0:
            # add classes_out, unless it's already in the classpath
            classpath = [os.path.abspath(p) for p in original_classpath]
            if classes_out not in classpath:
                classpath = [classes_out] + classpath
                # remove models.jar; it's added by the frontend
                models_jar = os.path.abspath(config.MODELS_JAR)
                if models_jar in classpath:
                    classpath.remove(models_jar)
            return ':'.join(classpath)
        return classes_out

    def _run_infer_frontend(self):
        infer_cmd = [utils.get_cmd_in_bin_dir('InferJava')]
        infer_cmd += ['-classpath', self._create_frontend_classpath()]
        infer_cmd += ['-class_source_map', self.javac.class_source_map]

        if not self.args.absolute_paths:
            infer_cmd += ['-project_root', self.args.project_root]

        infer_cmd += [
            '-results_dir', self.args.infer_out,
            '-verbose_out', self.javac.verbose_out,
            '-suppress_warnings_out', self.javac.suppress_warnings_out,
        ]

        if os.path.isfile(config.MODELS_JAR):
            infer_cmd += ['-models', config.MODELS_JAR]

        infer_cmd.append('-no-static_final')

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
