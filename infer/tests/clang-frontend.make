# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

ROOT_DIR = $(TESTS_DIR)/../..
include $(ROOT_DIR)/Makefile.config

default: compile

test: capture
	set -e; \
	for file in $(SOURCES) ; do \
    diff -u $$file.dot $$file.test.dot ; \
	done
	$(MAKE) clean

replace: capture
	for file in $(SOURCES) ; do \
    mv $$file.test.dot $$file.dot ; \
	done
	$(MAKE) clean

clean:
	rm -rf infer-out *.o */*.test.dot */*/*.test.dot
