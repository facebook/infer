# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

ROOT_DIR = ../../../../..
include $(ROOT_DIR)/Makefile.config

default: compile

.PHONY: print replace test clean
print replace test clean:
	$(QUIET)INFER_ARGS=--python-exe^$(PYTHONNEXT) $(MAKE) -C ../../python/pulse TEST_SUFFIX=-next TEST_RESULT_SUFFIX=-next ROOT_REL_SUFFIX=-next $@
