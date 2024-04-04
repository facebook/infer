#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e

infer -- make -C 00 clean all
infer -- make -C 01 clean all
infer explore --no-source-preview --select 1
infer -- make -C 02 clean all
infer explore --no-source-preview --select 0
infer -- make -C 03 clean all
infer explore --no-source-preview --select 1
