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

import config
import utils

FILELISTS = 'filelists'

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


class AnnotationProcessorNotFound(Exception):

    def __init__(self, path):
        self.path = path

    def __str__(self):
        return repr(self.path + ' not found')


class CompilerCall:

    def __init__(self, arguments):

        self.original_arguments = arguments
        self.args, self.remaining_args = parser.parse_known_args(arguments)
        self.verbose_out = None
        self.annotations_out = None

    def run(self):
        if self.args.version:
            return subprocess.call(['javac'] + self.original_arguments)
        else:
            javac_cmd = ['javac', '-verbose', '-g']

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
                self.annotations_out = annot_out.name

            with tempfile.NamedTemporaryFile(
                    mode='w',
                    suffix='.out',
                    prefix='javac_',
                    delete=False) as file_out:
                self.verbose_out = file_out.name
                os.environ['INFER_ANNOTATIONS_OUT'] = self.annotations_out
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
                    utils.error(error_msg.format(failing_cmd))
                    subprocess.check_call(failing_cmd)

        return os.EX_OK
