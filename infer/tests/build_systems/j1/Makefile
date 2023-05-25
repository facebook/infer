# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

ROOT_DIR = ../../../..
include $(ROOT_DIR)/Makefile.config

default: test

.PHONY: print replace test clean
print replace test clean:
	$(QUIET)INFER_ARGS=-j^1 $(MAKE) -C ../../codetoanalyze/cpp/siof TEST_SUFFIX=-j1 $@
