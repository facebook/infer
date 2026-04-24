# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
TESTS_DIR = ../../..

# Redefine only the checker-specific analysis options
INFER_OPTIONS = \
  --swift-objc-nullability-only --debug-exceptions --project-root $(abspath .) \
  --jobs 1

# Pull in the logic and defaults
include $(TESTS_DIR)/swift-interop.make
