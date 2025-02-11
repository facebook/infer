# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from cyclic_import import B

class A:
    pass

class B(A):
    pass


class C(B):
    pass
