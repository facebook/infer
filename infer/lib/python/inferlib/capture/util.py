#!/usr/bin/env python

# Copyright (c) Facebook, Inc. and its affiliates.
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
    (out_chars, err_chars) = proc.communicate()
    out = utils.decode(out_chars) if out_chars is not None else ''
    err = utils.decode(err_chars) if err_chars is not None else ''
    if proc.returncode != os.EX_OK:
        utils.stderr(
            'ERROR: couldn\'t run compilation command `{}`'.format(build_cmd))
        logging.error(
            'ERROR: couldn\'t run compilation command `{}`:\n\
            *** stdout:\n{}\n*** stderr:\n{}\n'
            .format(build_cmd, out, err))
    return (proc.returncode, (out, err))


def run_compilation_commands(cmds):
    """runs all the commands passed as argument
    """
    #  TODO call it in parallel
    if cmds is None or len(cmds) == 0:
        # nothing to capture, the OCaml side will detect that and
        # display the appropriate warning
        return os.EX_OK
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
