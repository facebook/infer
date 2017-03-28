# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..

ANALYZER = infer
SOURCES = $(wildcard src/infer/*.java)
OBJECTS = $(patsubst %.java,ant_out/%.class,$(SOURCES))
INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/java.make
include $(TESTS_DIR)/infer.make

$(OBJECTS): $(SOURCES)
	$(ANT)

infer-out/report.json: $(JAVA_DEPS) $(SOURCES)
	$(QUIET)$(call silent_on_success,Testing ant integration,\
	  $(INFER_BIN) -a $(ANALYZER) --project-root $(TESTS_DIR) --inferconfig-home . -- $(ANT))

clean:
	$(ANT) clean
