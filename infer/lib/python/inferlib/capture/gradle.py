# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import itertools
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
    size = len(parts)
    pos = size - 1
    while pos >= 0:
        path = ' '.join(itertools.islice(parts, pos, None))
        if os.path.isfile(path):
            return parts[:pos], path
        pos -= 1
    return parts, None


def pop(the_list):
    if len(the_list) > 0:
        return the_list.pop()
    return None


def extract_argfiles_from_rev(javac_arguments):
    """Extract class names and @argfiles from the reversed list."""
    # Reverse the list, so it's in a natural order now
    javac_arguments = list(reversed(javac_arguments))
    java_opts = []
    saved = []
    java_arg = pop(javac_arguments)
    while java_arg is not None:
        if java_arg.startswith('@'):
            # Probably got an @argfile
            path = ' '.join([java_arg[1:]] + saved)
            if os.path.isfile(path):
                java_opts.insert(0, '@' + path)
                saved = []
            else:
                # @ at the middle of the path
                saved.insert(0, java_arg)
        else:
            # Either a class name or a part of the @argfile path
            saved.insert(0, java_arg)
        java_arg = pop(javac_arguments)

    # Only class names left
    java_opts[0:0] = saved

    return java_opts


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
    {'files': [], 'opts': ['undef1', 'undef2']}
    >>> extract_all(['-o', '/path/to/1.java', 'cls.class', '@/path/to/1'])
    {'files': ['/path/to/1.java'], 'opts': ['-o', 'cls.class', '@/path/to/1']}
    >>> extract_all(['-opt1', 'optval1', '/path/to/1.java', 'cls.class'])
    {'files': ['/path/to/1.java'], 'opts': ['-opt1', 'optval1', 'cls.class']}
    >>> extract_all(['cls.class', '@/path/to/a', 'b.txt'])
    {'files': [], 'opts': ['cls.class', '@/path/to/a b.txt']}
    >>> extract_all(['cls.class', '@/path/to/a', '@b.txt'])
    {'files': [], 'opts': ['cls.class', '@/path/to/a @b.txt']}
    >>> v = extract_all(['-opt1', 'optval1'] * 1000 + ['/path/to/1.java'])
    >>> len(v['opts'])
    2000
    """
    java_files = []
    java_opts = []
    # Reversed Javac options parameters
    rev_opt_params = []
    java_arg = pop(javac_arguments)
    while java_arg is not None:
        if java_arg.endswith('.java'):
            # Probably got a file
            remainder, path = extract_filepath(javac_arguments + [java_arg])
            if path is not None:
                java_files.append(path)
                javac_arguments = remainder
                # The file name can't be in the middle of the option
                java_opts.extend(extract_argfiles_from_rev(rev_opt_params))
                rev_opt_params = []
            else:
                # A use-case here: *.java dir as an option parameter
                rev_opt_params.append(java_arg)
        elif java_arg.startswith('-'):
            # Got a Javac option
            option = [java_arg]
            if len(rev_opt_params) > 0:
                option.append(' '.join(reversed(rev_opt_params)))
                rev_opt_params = []
            java_opts[0:0] = option
        else:
            # Got Javac option parameter
            rev_opt_params.append(java_arg)
        java_arg = pop(javac_arguments)

    # We may have class names and @argfiles besides java files and options
    java_opts.extend(extract_argfiles_from_rev(rev_opt_params))

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
        for line in verbose_output.split('\n'):
            if argument_start_pattern in line:
                content = line.partition(argument_start_pattern)[2].strip()
                # if we're building both the debug and release configuration
                # and the build commands are identical up to "release/debug",
                # only do capture for one set of commands
                build_agnostic_cmd = content.replace('release', 'debug')
                if build_agnostic_cmd in seen_build_cmds:
                    continue
                seen_build_cmds.add(build_agnostic_cmd)
                arguments = content.split(' ')
                # Note: do *not* try to filter out empty strings from the arguments (as was done
                # here previously)! It will make compilation commands like
                # `javac -classpath '' -Xmaxerrs 1000` fail with "Unrecognized option 1000"
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
        (build_code, (verbose_out, _)) = util.get_build_output(self.build_cmd)
        cmds = self.get_infer_commands(verbose_out)
        capture_code = util.run_compilation_commands(cmds)
        if build_code != os.EX_OK:
            return build_code
        return capture_code
