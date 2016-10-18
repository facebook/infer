# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

import make
import util

MODULE_NAME = 'ndk-build/clang'
MODULE_DESCRIPTION = '''Run analysis of code built with ndk-build

    Analysis examples:
    infer -- ndk-build'''
LANG = ['clang']


def gen_instance(*args):
    return NdkBuildCapture(*args)


create_argparser = util.base_argparser(MODULE_DESCRIPTION, MODULE_NAME)


class NdkBuildCapture(make.MakeCapture):
    def __init__(self, args, cmd):
        cmd = [cmd[0], 'NDK_TOOLCHAIN_VERSION=clang', 'TARGET_CC=clang',
               'TARGET_CXX=clang++', 'TARGET_LD=ld'] + cmd[1:]
        make.MakeCapture.__init__(self, args, cmd)
