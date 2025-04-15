# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from not_captured import id_not_captured
from dir1.dir3.testmod import id as id_captured
from dir1.dir4.skipped import id as id_skipped

#expected since id_not_captured is not captured
def FP_assert_true_with_id_not_captured_ok():
    assert id_not_captured(True)


def assert_false_with_id_not_captured_bad():
    assert id_not_captured(False)


def assert_true_with_id_captured_ok():
    assert id_captured(True)


def assert_false_with_id_captured_bad():
    assert id_captured(False)


#expected since id_skipped is not captured (see .inferconfig and option
def FP_assert_true_with_id_skipped_ok():
    assert id_skipped(True)


def assert_false_with_id_skipped_bad():
    assert id_skipped(False)
