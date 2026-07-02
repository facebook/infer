# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Any, Dict, Optional


class C:
    def m(self, b: Optional[Dict[str, Any]] = None):
        if b is None:
            b = {}
        return len(b)
