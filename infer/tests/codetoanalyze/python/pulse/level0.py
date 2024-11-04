# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

def taint_sink(x):
    pass

def taint_source():
    pass

def no_taint_source():
    pass

taint_sink(taint_source())

taint_sink(no_taint_source())
