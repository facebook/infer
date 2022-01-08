# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

INFER_OPTIONS = \
  --cfg-json cfg.json \
  --tenv-json tenv.json \

INFERPRINT_OPTIONS = --issues-tests-fields "file,procedure,line_offset,bug_type,bucket" --issues-tests

include $(TESTS_DIR)/analyzejson.make
