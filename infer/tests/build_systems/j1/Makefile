# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

ROOT_DIR = ../../../..
include $(ROOT_DIR)/Makefile.config

default: compile

.PHONY: print replace test clean
print replace test clean:
	$(QUIET)INFER_ARGS=-j^1 $(MAKE) -C ../../codetoanalyze/cpp/siof TEST_SUFFIX=-j1 $@
