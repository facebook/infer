# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

ANALYZER = checkers
SOURCES = $(wildcard src/infer/*.java)
OBJECTS = $(patsubst %.java,ant_out/%.class,$(SOURCES))
INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/java.make
include $(TESTS_DIR)/infer.make

$(OBJECTS): $(SOURCES)
	$(ANT)

infer-out/report.json: $(JAVA_DEPS) $(SOURCES)
	$(QUIET)$(call silent_on_success,Testing ant integration,\
	  $(INFER_BIN) -a $(ANALYZER) --project-root $(TESTS_DIR) -- $(ANT))

clean:
	$(ANT) clean
