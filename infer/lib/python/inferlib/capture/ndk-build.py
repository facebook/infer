# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

import os
import shutil

import make
import util
from inferlib import config

MODULE_NAME = 'ndk-build/clang'
MODULE_DESCRIPTION = '''Run analysis of code built with ndk-build

    Analysis examples:
    infer -- ndk-build'''


def gen_instance(*args):
    return NdkBuildCapture(*args)


create_argparser = \
    util.clang_frontend_argparser(MODULE_DESCRIPTION, MODULE_NAME)


class NdkBuildCapture(make.MakeCapture):
    def __init__(self, args, cmd):
        make.MakeCapture.__init__(self, args, cmd)

    def get_envvars(self):
        env_vars = make.MakeCapture.get_envvars(self)
        env_vars['NDK_TOOLCHAIN'] = 'infer'
        return env_vars

    def _get_android_ndk_dir(self):
        # TODO: other ways to get android_ndk
        android_ndk = self.get_envvars()['ANDROID_NDK']
        if not os.path.exists(android_ndk):
            raise Exception('Cannot find the location of the Android NDK. '
                            'Please set the ANDROID_NDK environment variable '
                            'to a path containing an installation of the '
                            'Android NDK.')
        return android_ndk

    def _setup_infer_toolchain(self):
        src_dir = os.path.join(config.LIB_DIRECTORY, 'ndk_capture', 'infer')
        android_ndk = self._get_android_ndk_dir()
        dest_dir = os.path.join(android_ndk, 'toolchains', 'infer')
        if not os.path.exists(dest_dir):
            try:
                shutil.copytree(src_dir, dest_dir)
            except shutil.Error:
                raise Exception('An error occurred while setting up infer '
                                'toolchain in Android NDK. Please ensure '
                                'infer has write access to the '
                                '$ANDROID_NDK/toolchains directory.')

    def _cleanup_infer_toolchain(self):
        android_ndk = self._get_android_ndk_dir()
        infer_toolchain = os.path.join(android_ndk, 'toolchains', 'infer')
        if os.path.exists(infer_toolchain):
            try:
                shutil.rmtree(infer_toolchain)
            except shutil.Error:
                raise Exception('An error occurred while cleaning up infer '
                                'toolchain in Android NDK. Please ensure '
                                'infer has write access to the '
                                '$ANDROID_NDK/toolchains directory.')

    def capture(self):
        try:
            self._setup_infer_toolchain()
            return make.MakeCapture.capture(self)
        finally:
            self._cleanup_infer_toolchain()
