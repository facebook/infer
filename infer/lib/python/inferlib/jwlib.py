# Copyright (c) 2009-2013, Monoidics ltd.
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import os
import subprocess
from . import config


class InferJavacCapture():

    def __init__(self, javac_args):
        self.javac_args = javac_args

    def start(self):
        infer = os.path.join(config.BIN_DIRECTORY, 'infer')
        # pass --continue to prevent removing the results-dir
        cmd = [
            infer,
            'capture',
            '--continue',
            '--', 'javac'
        ] + self.javac_args
        try:
            return subprocess.check_call(cmd)
        except Exception as e:
            print('Failed to execute:', ' '.join(cmd))
            raise e


def _get_javac_args(javac_args):
    # replace any -g:.* flag with -g to preserve debugging symbols
    args = map(lambda arg: '-g' if '-g:' in arg else arg, javac_args)
    # skip -Werror
    args = filter(lambda arg: arg != '-Werror', args)
    return args


def create_infer_command(javac_args):
    return InferJavacCapture(_get_javac_args(javac_args))
