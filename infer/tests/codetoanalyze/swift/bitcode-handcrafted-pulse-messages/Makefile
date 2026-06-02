# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Message-text tests driven from handcrafted LLVM IR. This is the
# handcrafted-LL counterpart of `pulse-messages/` (which compiles `.swift`
# sources): it takes pre-built `.swift.ll` artifacts as input and pins the
# user-facing qualifier (+ suggestion) text, not just the issue location.
# Use it when a message-rendering change needs an IR shape the local
# swiftc + `bitcode/` pipeline can't reproduce.
#
# Kept separate from `bitcode-handcrafted-pulse/` on purpose: that
# directory pins issue locations only (`--issues-tests`) and is shared with
# the cycle-detection corpus. Mixing qualifier text into it would couple
# detection fixtures to message wording.
#
# Each `.swift.ll` is paired with a `.swift.source` file naming the path to
# use as `--llvm-bitcode-source`. The path MUST match the `!DIFile` debug
# entry inside the `.ll` so that `Llair2Textual.should_translate` accepts
# the captured source-file functions.

TESTS_DIR = ../../..

INFER_OPTIONS = \
  --pulse-only --debug-exceptions --project-root $(TESTS_DIR) \
  --pulse-report-issues-for-tests --jobs 1

INFERPRINT_OPTIONS = \
  --issues-tests-fields file,procedure,line,column,severity,bug_type,bug_trace,qualifier,suggestion \
  --issues-tests

SOURCES = $(realpath $(wildcard *.swift.ll))

include $(TESTS_DIR)/infer.make

infer-out/report.json: $(SOURCES) $(MAKEFILE_LIST)
	$(QUIET)$(call silent_on_success,Running pulse on handcrafted LL in $(TEST_REL_DIR),\
	  $(REMOVE_DIR) $(@D) && \
	  first=1 ; \
	  for src in $(SOURCES) ; do \
	    base=$${src%.ll} ; \
	    src_path=$$(cat $$base.source) ; \
	    if [ $$first -eq 1 ] ; then continue_flag="" ; first=0 ; else continue_flag="--continue" ; fi ; \
	    $(INFER_BIN) --results-dir $(@D) $(INFER_OPTIONS) capture $$continue_flag \
	      --llvm-bitcode-file $$src --llvm-bitcode-source $$src_path ; \
	  done ; \
	  $(INFER_BIN) --results-dir $(@D) $(INFER_OPTIONS) analyze )
