# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Any, Dict


class C:
    def m(self, b: Dict[str, Any] = {}):
        return len(b)
