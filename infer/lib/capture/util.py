#!/usr/bin/env python

# Copyright (c) 2013- Facebook.
# All rights reserved.

import argparse
import os
import subprocess
import inferlib


def create_inferJ_command(args, javac_arguments):
    infer_args = ['-o', args.infer_out]
    if args.debug:
        infer_args.append('--debug')
    infer_args += ['--analyzer', 'capture']

    return inferlib.Infer(inferlib.inferJ_parser.parse_args(infer_args),
                          inferlib.get_javac_args(['javac'] + javac_arguments))


def get_build_output(build_cmd):
    #  TODO make it return generator to be able to handle large builds
    proc = subprocess.Popen(build_cmd, stdout=subprocess.PIPE)
    (verbose_out_chars, _) = proc.communicate()
    return verbose_out_chars.split('\n')


def run_commands(cmds):
    #  TODO call it in parallel
    if len(cmds) == 0:
        return os.EX_NOINPUT
    for cmd in cmds:
        if not cmd.start():
            return os.EX_SOFTWARE
    return os.EX_OK


def base_argparser(description, module_name):
    def _func(group_name=module_name):
        """This creates an empty argparser for the module, which provides only
        description/usage information and no arguments."""
        parser = argparse.ArgumentParser(add_help=False)
        group = parser.add_argument_group(
            "{grp} module".format(grp=group_name),
            description=description,
        )
        return parser
    return _func
