# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..
INFER_OUT ?= infer-out$(TEST_SUFFIX)

include $(TESTS_DIR)/base.make
include $(TESTS_DIR)/java.make

SOURCES = $(sort $(wildcard *.java))
SIL_FILES = $(patsubst %.java,%.sil,$(SOURCES))
LOG_FILES = $(patsubst %.java,%.log,$(SOURCES))

CLASSES_DIR = classes
# infer-outs from --dump-textual
DUMP_DIR = dump-outs
# infer-outs from --capture-textual
CAPTURE_DIR = capture-outs

CLEAN_EXTRA = $(CLASSES_DIR) $(DUMP_DIR) $(CAPTURE_DIR) *.sil *.log

$(CLASSES_DIR):
	mkdir $@

%.sil: %.java $(CLASSES_DIR) $(INFER_BIN)
	$(INFER_BIN) -g capture -o "$(DUMP_DIR)/infer-out-$(basename $<)" --dump-textual -- javac -cp $(CLASSPATH) -d $(CLASSES_DIR) $<

.PHONY: compile
compile: $(SIL_FILES)

%.log: %.sil $(INFER_BIN)
	$(INFER_BIN) --pulse-only --no-pulse-force-continue --quiet --no-progress-bar \
	  -o "$(CAPTURE_DIR)/infer-out-$(basename $<)" --capture-textual $< &> $@ \
	|| exit 0

issues.exp.test: $(LOG_FILES)
	cat $^ | awk '{gsub("$(realpath $(shell pwd))", "");print}' > $@

.PHONY: test
test: issues.exp.test
	$(QUIET)cd $(TESTS_DIR) && \
	$(call check_no_diff,$(TEST_REL_DIR)/issues.exp$(TEST_RESULT_SUFFIX),$(TEST_REL_DIR)/issues.exp.test$(TEST_SUFFIX))

.PHONY: replace
replace: issues.exp.test
	cp $< issues.exp

.PHONY: clean
clean:
	$(REMOVE_DIR) issues.exp.test$(TEST_SUFFIX) $(CLASS_FILES) $(CLEAN_EXTRA)
ifneq ($(INFER_OUT),.)
	$(REMOVE_DIR) $(INFER_OUT)
endif
