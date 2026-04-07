# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-ignore-all-errors

from infer_semdiff import accept, var

T1 = var("T1")
T2 = var("T2")

# accept any change to the GRAPHQL_ARTIFACT_TEXT constant value
# (e.g. adding @owner directive)
accept(
    lhs=constant_declarator(
        constant_declarator_name="GRAPHQL_ARTIFACT_TEXT",
        constant_declarator_initializer=T1,
    ),
    rhs=constant_declarator(
        constant_declarator_name="GRAPHQL_ARTIFACT_TEXT",
        constant_declarator_initializer=T2,
    ),
)
