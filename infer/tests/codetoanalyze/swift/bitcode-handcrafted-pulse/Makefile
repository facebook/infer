# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Pulse tests driven from handcrafted LLVM IR. Mirrors the pulse/ test
# corpus but takes pre-built `.swift.ll` artifacts as input instead of
# compiling `.swift` sources through swiftc, so we can exercise lowerings
# the local swiftc + `bitcode/` pipeline does not faithfully reproduce.
#
# Each `.swift.ll` is paired with a `.swift.source` file that names the
# path to use as `--llvm-bitcode-source`. The path MUST match the `!DIFile`
# debug entry inside the `.ll` so that `Llair2Textual.should_translate`
# accepts the captured source-file functions.

TESTS_DIR = ../../..

INFER_OPTIONS = \
  --pulse-only --debug-exceptions --project-root $(TESTS_DIR) \
  --pulse-report-issues-for-tests --jobs 1

INFERPRINT_OPTIONS = --issues-tests

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
