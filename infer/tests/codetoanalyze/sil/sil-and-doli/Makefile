# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

include $(TESTS_DIR)/base.make


SOURCES = $(sort $(wildcard *.sil))  $(sort $(wildcard *.doli))
INFER_SETTINGS = -j 1 --no-progress-bar --pulse-only   -o infer-out

SIL_GROUP_1 =  callingExternals_ret_null_TD.sil external_ret_null_t.sil
DOLI_GROUP_1 =  external_ret_null_d.doli

SIL_GROUP_2 = callingExternals_Several.sil
DOLI_GROUP_2 = externals_Some.doli

SIL_GROUP_3 =
DOLI_GROUP_3 = callingInternals.doli

SIL_GROUP_4 = external_ret_this.sil
DOLI_GROUP_4 = callingTextualFromDoli.doli

CAPTURE_CMD_1 = $(patsubst %.sil, --capture-textual %.sil, $(SIL_GROUP_1)) \
	$(patsubst %.doli, --capture-doli %.doli, $(DOLI_GROUP_1))
CAPTURE_CMD_2 = $(patsubst %.sil, --capture-textual %.sil, $(SIL_GROUP_2)) \
	 $(patsubst %.doli, --capture-doli %.doli, $(DOLI_GROUP_2))
CAPTURE_CMD_3 = $(patsubst %.sil, --capture-textual %.sil, $(SIL_GROUP_3)) \
	$(patsubst %.doli, --capture-doli %.doli, $(DOLI_GROUP_3))
CAPTURE_CMD_4 = $(patsubst %.sil, --capture-textual %.sil, $(SIL_GROUP_4)) \
	 $(patsubst %.doli, --capture-doli %.doli, $(DOLI_GROUP_4))

CAPTURE_CMDS =  "$(CAPTURE_CMD_1)" \
				"$(CAPTURE_CMD_2)" \
				"$(CAPTURE_CMD_3)" \
				"$(CAPTURE_CMD_4)"

.PHONY: compile
compile: issues.exp.test
issues.exp.test: $(SOURCES) $(INFER_BIN)
	> $@
	@for cmd in $(CAPTURE_CMDS); do \
		echo $$cmd >> $@; \
		$(INFER_BIN) $(INFER_SETTINGS) $$cmd; \
		$(INFER_BIN) report -o infer-out --issues-tests tmp; \
		cat tmp >> $@; \
		echo ____________________________________  >> $@; \
    done
	$(QUIET)$(REMOVE) tmp


.PHONY: test
test: issues.exp.test
	$(QUIET)cd $(TESTS_DIR) && \
	 $(call check_no_diff,$(TEST_REL_DIR)/issues.exp$(TEST_RESULT_SUFFIX),$(TEST_REL_DIR)/issues.exp.test$(TEST_SUFFIX))

.PHONY: replace
replace: issues.exp.test
	cp $< issues.exp

clean:
	$(QUIET)$(REMOVE_DIR) infer-out issues.exp.test
