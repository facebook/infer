# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import random
import taint

untained = random.random()

# bad
taint.sink(taint.source())

# ok
taint.sink(random.random())

tuple = (taint.source(), random())

# bad
taint.sink(tuple[0])

# ok
taint.sink(tuple[1])
