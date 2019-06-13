# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

SOURCES = $(wildcard src/infer/*.java)
OBJECTS = $(patsubst %,ant_out/codetoanalyze/java/infer/%,$(notdir $(patsubst %.java,%.class,$(SOURCES))))
CLASS_FILES = $(notdir $(patsubst %.java,%.class,$(SOURCES)))
OBJECTS = $(CLASS_FILES:%=ant_out/codetoanalyze/java/infer/%)
INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/java.make
include $(TESTS_DIR)/infer.make

$(OBJECTS): $(SOURCES)
	$(ANT)

infer-out/report.json: $(JAVA_DEPS) $(SOURCES)
	$(QUIET)$(call silent_on_success,Testing ant integration,\
	  $(INFER_BIN) --project-root $(TESTS_DIR) -- $(ANT))

clean:
	$(REMOVE_DIR) ant_out
