# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

import logging
import os
import util
import tempfile

MODULE_NAME = __name__
MODULE_DESCRIPTION = '''Run analysis of code built with a command like:
gradle [options] [task]

Analysis examples:
infer -- gradle build
infer -- ./gradlew build'''
LANG = ['java']


def gen_instance(*args):
    return GradleCapture(*args)


# This creates an empty argparser for the module, which provides only
# description/usage information and no arguments.
create_argparser = util.base_argparser(MODULE_DESCRIPTION, MODULE_NAME)


def extract_filepath(parts):
    if not parts:
        return ([], None)
    path = ' '.join(parts)
    if os.path.isfile(path):
        return ([], path)
    remainder, path = extract_filepath(parts[1:])
    return ([parts[0]] + remainder, path)


# Please run the doctests using:
# $ python -m doctest -v gradle.py
def extract_all(javac_arguments):
    """Extract Java filenames and Javac options from the Javac arguments.

    >>> os.path.isfile = lambda s: s[1:].startswith('path/to/')
    >>> extract_all([])
    {'files': [], 'opts': []}
    >>> extract_all(['-opt1', 'optval1', '/path/to/1.java'])
    {'files': ['/path/to/1.java'], 'opts': ['-opt1', 'optval1']}
    >>> extract_all(['-opt1', 'optval1', '/path/to/a', 'b/1.java'])
    {'files': ['/path/to/a b/1.java'], 'opts': ['-opt1', 'optval1']}
    >>> extract_all(['-opt1', 'opt', 'val1', '/path/to/1.java'])
    {'files': ['/path/to/1.java'], 'opts': ['-opt1', 'opt val1']}
    >>> extract_all(['-opt1', '/path/to/a', 'b/c', 'd/1.java', '-opt2'])
    {'files': ['/path/to/a b/c d/1.java'], 'opts': ['-opt1', '-opt2']}
    >>> extract_all(['-opt1', 'optval1', '-path/to/1.java'])
    {'files': ['-path/to/1.java'], 'opts': ['-opt1', 'optval1']}
    >>> extract_all(['-opt1', 'optval1', '/path/to/', '-1.java'])
    {'files': ['/path/to/ -1.java'], 'opts': ['-opt1', 'optval1']}
    >>> extract_all(['undef1', 'undef2'])
    {'files': [], 'opts': ['undef1 undef2']}
    """
    def pop():
        if javac_arguments:
            return javac_arguments.pop()
        return None

    java_files = []
    java_opts = []
    # Reversed Javac options parameters
    rev_opt_params = []
    java_arg = pop()
    while java_arg:
        if java_arg.endswith('.java'):
            # Probably got a file
            remainder, path = extract_filepath(javac_arguments + [java_arg])
            if path:
                java_files.append(path)
                javac_arguments = remainder
            else:
                # A use-case here: *.java dir as an option parameter
                rev_opt_params.append(java_arg)
        elif java_arg[0] == '-':
            # Got a Javac option
            option = [java_arg]
            if rev_opt_params:
                option.append(' '.join(reversed(rev_opt_params)))
                rev_opt_params = []
            java_opts[0:0] = option
        else:
            # Got Javac option parameter
            rev_opt_params.append(java_arg)
        java_arg = pop()
    if rev_opt_params:
        # Javac option without - (or incorrect file), put into Javac arguments
        option = [' '.join(reversed(rev_opt_params))]
        java_opts[0:0] = option
    return {'files': java_files, 'opts': java_opts}


def normalize(path):
    from inferlib import utils
    # From Javac docs: If a filename contains embedded spaces,
    # put the whole filename in double quotes
    quoted_path = path
    if ' ' in path:
        quoted_path = '"' + path + '"'
    return utils.encode(quoted_path)


class GradleCapture:

    def __init__(self, args, cmd):
        from inferlib import config, utils

        self.args = args
        # TODO: make the extraction of targets smarter
        self.build_cmd = [cmd[0], '--debug'] + cmd[1:]
        # That contains javac version as well
        version_str = util.run_cmd_ignore_fail([cmd[0], '--version'])
        path = os.path.join(self.args.infer_out,
                            config.JAVAC_FILELISTS_FILENAME)
        if not os.path.exists(path):
            os.mkdir(path)
        logging.info('Running with:\n' + utils.decode(version_str))

    def get_infer_commands(self, verbose_output):
        from inferlib import config, jwlib

        argument_start_pattern = ' Compiler arguments: '
        calls = []
        seen_build_cmds = set([])
        for line in verbose_output:
            if argument_start_pattern in line:
                content = line.partition(argument_start_pattern)[2].strip()
                # if we're building both the debug and release configuration
                # and the build commands are identical up to "release/debug",
                # only do capture for one set of commands
                build_agnostic_cmd = content.replace('release', 'debug')
                if build_agnostic_cmd in seen_build_cmds:
                    continue
                seen_build_cmds.add(build_agnostic_cmd)
                # Filter out the empty elements
                arguments = list(filter(None, content.split(' ')))
                extracted = extract_all(arguments)
                java_files = extracted['files']
                java_args = extracted['opts']

                with tempfile.NamedTemporaryFile(
                        mode='w',
                        suffix='.txt',
                        prefix='gradle_',
                        dir=os.path.join(self.args.infer_out,
                                         config.JAVAC_FILELISTS_FILENAME),
                        delete=False) as sources:
                    sources.write('\n'.join(map(normalize, java_files)))
                    sources.flush()
                    java_args.append('@' + sources.name)
                capture = jwlib.create_infer_command(java_args)
                calls.append(capture)
        return calls

    def capture(self):
        print('Running and capturing gradle compilation...')
        cmds = self.get_infer_commands(util.get_build_output(self.build_cmd))
        clean_cmd = '%s clean' % self.build_cmd[0]
        return util.run_compilation_commands(cmds, clean_cmd)
