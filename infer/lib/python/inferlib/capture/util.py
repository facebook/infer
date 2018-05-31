#!/usr/bin/env python

# Copyright (c) 2015-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import argparse
import os
import logging
import subprocess
import traceback


def get_build_output(build_cmd):
    from inferlib import utils
    #  TODO make it return generator to be able to handle large builds
    proc = subprocess.Popen(build_cmd, stdout=subprocess.PIPE)
    (verbose_out_chars, _) = proc.communicate()
    if proc.returncode != 0:
        utils.stderr(
            'ERROR: couldn\'t run compilation command `{}`'.format(build_cmd))
        return (proc.returncode, None)
    out = utils.decode(verbose_out_chars).split('\n')
    return (os.EX_OK, out)


def run_compilation_commands(cmds, clean_cmd):
    """runs compilation commands, and suggests a project cleaning command
    in case there is nothing to compile.
    """
    from inferlib import utils
    #  TODO call it in parallel
    if cmds is None or len(cmds) == 0:
        utils.stderr('Nothing to compile. Try running `{}` first.'
                     .format(clean_cmd))
        return os.EX_NOINPUT
    for cmd in cmds:
        if cmd.start() != os.EX_OK:
            return os.EX_SOFTWARE
    return os.EX_OK


def run_cmd_ignore_fail(cmd):
    try:
        return subprocess.check_output(cmd, stderr=subprocess.STDOUT)
    except:
        return 'calling {cmd} failed\n{trace}'.format(
            cmd=' '.join(cmd),
            trace=traceback.format_exc())


def log_java_version():
    java_version = run_cmd_ignore_fail(['java', '-version'])
    javac_version = run_cmd_ignore_fail(['javac', '-version'])
    logging.info('java versions:\n%s%s', java_version, javac_version)


def base_argparser(description, module_name):
    def _func(group_name=module_name):
        """This creates an empty argparser for the module, which provides only
        description/usage information and no arguments."""
        parser = argparse.ArgumentParser(add_help=False)
        parser.add_argument_group(
            '{grp} module'.format(grp=group_name),
            description=description,
        )
        return parser
    return _func
