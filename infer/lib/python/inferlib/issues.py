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


ISSUE_KIND_ERROR = 'ERROR'
ISSUE_KIND_WARNING = 'WARNING'
ISSUE_KIND_INFO = 'INFO'

ISSUE_TYPES = [
    'ASSERTION_FAILURE',
    'BAD_POINTER_COMPARISON',
    # 'CHECKERS_PRINTF_ARGS'
    # TODO (#8030397): revert this once all the checkers are moved to Infer
    'CONTEXT_LEAK',
    'MEMORY_LEAK',
    'RESOURCE_LEAK',
    'RETAIN_CYCLE',
    'STRONG_DELEGATE_WARNING',
    'TAINTED_VALUE_REACHING_SENSITIVE_FUNCTION',
    'IVAR_NOT_NULL_CHECKED',
    'NULL_DEREFERENCE',
    'PARAMETER_NOT_NULL_CHECKED',
    'PREMATURE_NIL_TERMINATION_ARGUMENT',
]

NULL_STYLE_ISSUE_TYPES = [
    'IVAR_NOT_NULL_CHECKED',
    'NULL_DEREFERENCE',
    'PARAMETER_NOT_NULL_CHECKED',
    'PREMATURE_NIL_TERMINATION_ARGUMENT',
]
