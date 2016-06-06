# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

import os
import util

MODULE_NAME = __name__
MODULE_DESCRIPTION = '''Run analysis of what has already been captured:
Usage:
infer -- analyze
infer --out <capture_folder> -- analyze'''
LANG = ['clang', 'java']


def gen_instance(*args):
    return NoCapture(*args)

# This creates an empty argparser for the module, which provides only
# description/usage information and no arguments.
create_argparser = util.base_argparser(MODULE_DESCRIPTION, MODULE_NAME)


class NoCapture:
    def __init__(self, args, cmd):
        self.args = args

    def capture(self):
        return os.EX_OK
