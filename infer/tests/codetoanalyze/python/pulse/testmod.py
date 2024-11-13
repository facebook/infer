# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import random
import taint
from dir1.testmod import untainted_global1, tainted_global1
from dir1.dir3.testmod import untainted_global3, tainted_global3
from dir1.dir4.testmod import untainted_global4, tainted_global4
from dir2.testmod import untainted_global2, tainted_global2
from dir2.dir5.testmod import untainted_global5, tainted_global5
from dir2.dir6.testmod import untainted_global6, tainted_global6


def use1_bad():
    taint.sink(tainted_global1)


def use1_ok():
    taint.sink(untainted_global1)


def use2_bad():
    taint.sink(tainted_global2)


def use2_ok():
    taint.sink(untainted_global2)


def use3_bad():
    taint.sink(tainted_global3)


def use3_ok():
    taint.sink(untainted_global3)


def use4_bad():
    taint.sink(tainted_global4)


def use4_ok():
    taint.sink(untainted_global4)


def use5_bad():
    taint.sink(tainted_global5)


def use5_ok():
    taint.sink(untainted_global5)


def use6_bad():
    taint.sink(tainted_global6)


def use6_ok():
    taint.sink(untainted_global6)

use1_bad()


use1_ok()


use2_bad()


use2_ok()


use3_bad()


use3_ok()


use4_bad()


use4_ok()


use5_bad()


use5_ok()


use6_bad()


use6_ok()
