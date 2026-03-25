#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Lambda class names include a hash of the full source path, which changes
# when the file is copied to a temp directory. Replace the 8-char hex hash
# suffix with a stable placeholder.
# The hash appears after a line number: lambda_<file>_<ext>_<line>_<hash>
# or in strings: lambda_<file>.<ext>:<line>_<hash>
sed -E 's/(lambda_[A-Za-z0-9_.:]+[.:_][0-9]+)_[0-9a-f]{8}/\1_HASH/g'
