# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import locale


LOCALE = locale.getpreferredencoding()


# exit value when infer finds something to report
BUG_FOUND_ERROR_CODE = 2


# list of possible analyzers
ANALYZER_INFER = 'infer'
ANALYZER_ERADICATE = 'eradicate'
ANALYZER_CHECKERS = 'checkers'
ANALYZER_CAPTURE = 'capture'
ANALYZER_COMPILE = 'compile'
ANALYZER_TRACING = 'tracing'

ANALYZERS = [
    ANALYZER_CAPTURE,
    ANALYZER_CHECKERS,
    ANALYZER_COMPILE,
    ANALYZER_ERADICATE,
    ANALYZER_INFER,
    ANALYZER_TRACING,
]
