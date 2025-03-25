# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


def assert_true_bad():
    assert False


def assert_true_ok():
    assert True


# the assert-error issue is not latent: we consider such a partial
# function should be considered bad
def run_assert_latent_bad(b):
    assert b
