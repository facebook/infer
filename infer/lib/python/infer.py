#!/usr/bin/env python2.7

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import argparse
import imp
import locale
import logging
import os
import platform
import sys

import inferlib
from inferlib import analyze, config, utils

CAPTURE_PACKAGE = 'capture'

# token that identifies the end of the options for infer and the beginning
# of the compilation command
CMD_MARKER = '--'

# insert here the correspondence between module name and the list of
# compiler/build-systems it handles.
# All supported commands should be listed here
MODULE_TO_COMMAND = {
    'ant': ['ant'],
    'buck': ['buck'],
    'gradle': ['gradle', 'gradlew'],
    'xcodebuild': ['xcodebuild'],
    'ndk-build': ['ndk-build'],
}


def get_commands():
    """Return all commands that are supported."""
    # flatten and dedup the list of commands
    return set(sum(MODULE_TO_COMMAND.values(), []))


def get_module_name(command):
    """ Return module that is able to handle the command. None if
    there is no such module."""
    for module, commands in MODULE_TO_COMMAND.iteritems():
        if command in commands:
            return module
    return None


def load_module(mod_name):
    pkg_info = imp.find_module(CAPTURE_PACKAGE, inferlib.__path__)
    imported_pkg = imp.load_module(CAPTURE_PACKAGE, *pkg_info)
    # load the requested module (e.g. make)
    mod_file, mod_path, mod_descr = \
        imp.find_module(mod_name, imported_pkg.__path__)
    try:
        return imp.load_module(
            '{pkg}.{mod}'.format(pkg=imported_pkg.__name__, mod=mod_name),
            mod_file, mod_path, mod_descr)
    finally:
        if mod_file:
            mod_file.close()


def split_args_to_parse():
    sys_argv = map(utils.decode, sys.argv)
    dd_index = \
        sys_argv.index(CMD_MARKER) if CMD_MARKER in sys_argv else len(sys_argv)
    cmd_raw = sys_argv[dd_index + 1:]
    return (sys_argv[1:dd_index], cmd_raw)


class FailSilentlyArgumentParser(argparse.ArgumentParser):
    '''We want to leave the handling of printing usage messages to the
    OCaml code. To do so, swallow error messages from ArgumentParser
    and exit with a special error code that infer.ml looks for.
    '''

    def error(self, message):
        utils.stderr(message)
        utils.stderr('')
        exit(22)  # in sync with infer.ml

    def print_help(self, file=None):
        exit(22)  # in sync with infer.ml


def create_argparser(parents=[]):
    parser = FailSilentlyArgumentParser(
        parents=[analyze.infer_parser] + parents,
        add_help=False,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    group = parser.add_argument_group(
        'supported compiler/build-system commands')

    supported_commands = ', '.join(get_commands())
    group.add_argument(
        CMD_MARKER,
        metavar='<cmd>',
        dest='nullarg',
        default=None,
        help=('Command to run the compiler/build-system. '
              'Supported build commands (run `infer --help -- <cmd_name>` for '
              'extra help, e.g. `infer --help -- javac`): {}'.format(
                supported_commands)),
    )
    return parser


class IgnoreFailuresArgumentParser(argparse.ArgumentParser):
    def error(self, message):
        pass

    def print_help(self, file=None):
        pass

def main():
    to_parse, cmd = split_args_to_parse()
    # first pass to see if a capture module is forced
    initial_argparser = IgnoreFailuresArgumentParser(
        parents=[analyze.infer_parser],
        add_help=False,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    initial_args = initial_argparser.parse_args(to_parse)

    # get the module name (if any), then load it
    capture_module_name = None
    if initial_args.force_integration is not None:
        capture_module_name = initial_args.force_integration
    elif len(cmd) > 0:
        capture_module_name = os.path.basename(cmd[0])
    mod_name = get_module_name(capture_module_name)
    imported_module = None
    if mod_name:
        # There is module that supports the command
        imported_module = load_module(mod_name)

    # get the module's argparser and merge it with the global argparser
    module_argparser = []
    if imported_module:
        module_argparser.append(
            imported_module.create_argparser(capture_module_name)
        )
    global_argparser = create_argparser(module_argparser)

    args = global_argparser.parse_args(to_parse)

    if imported_module is not None:
        utils.configure_logging(args)
        try:
            logging.info('output of locale.getdefaultlocale(): %s',
                         str(locale.getdefaultlocale()))
        except (locale.Error, ValueError) as e:
            logging.info('locale.getdefaultlocale() failed with exception: %s',
                         str(e))
        logging.info('encoding we chose in the end: %s',
                     config.CODESET)
        logging.info('Running command %s',
                     ' '.join(map(utils.decode, sys.argv)))
        logging.info('Path to infer script %s (%s)', utils.decode(__file__),
                     os.path.realpath(utils.decode(__file__)))
        logging.info('Platform: %s', utils.decode(platform.platform()))

        def log_getenv(k):
            v = os.getenv(k)
            if v is not None:
                v = utils.decode(v)
            else:
                v = '<NOT SET>'
            logging.info('%s=%s', k, v)

        log_getenv('PATH')
        log_getenv('SHELL')
        log_getenv('PWD')

        capture_exitcode = imported_module.gen_instance(args, cmd).capture()
        if capture_exitcode != os.EX_OK:
            logging.error('Error during capture phase, exiting')
            exit(capture_exitcode)
        logging.info('Capture phase was successful')
    elif capture_module_name is not None:
        # There was a command, but it's not supported
        utils.stderr('Command "{cmd}" not recognised'
                     .format(cmd='' if capture_module_name is None
                             else capture_module_name))
        global_argparser.print_help()
        sys.exit(1)
    else:
        global_argparser.print_help()
        sys.exit(os.EX_OK)


if __name__ == '__main__':
    main()
