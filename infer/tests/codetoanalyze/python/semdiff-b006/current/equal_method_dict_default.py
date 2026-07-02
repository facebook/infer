# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


class C:
    def m(self, b=None):
        if b is None:
            b = {}
        return len(b)
