# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-ignore-all-errors

from infer_semdiff import accept, var

T1 = var("T1")
T2 = var("T2")

# accept any type change in type annotation positions
accept(
    lhs=T1,
    rhs=T2,
    key=[
        "function_type",
        "parameter_type",
        "property_type",
        "lambda_type",
        "anonymous_type",
    ],
)
