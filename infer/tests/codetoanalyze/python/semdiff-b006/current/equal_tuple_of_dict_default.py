# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


def f(fields=None):
    if fields is None:
        fields = (dict(key="img", stack=True), dict(key="seg"))
    return len(fields)
