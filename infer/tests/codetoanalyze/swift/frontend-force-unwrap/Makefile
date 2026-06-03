# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR=../../..

# Compile -Onone so the force-unwrap function bodies survive (the default
# optimised frontend compile DCEs these small pure functions to declarations).
# -Onone also matches the lowering the interop-pulse Pulse tests analyse, so the
# Textual SIL here shows exactly the trap rewrite Pulse sees.
SWIFT_OPTIONS = -Onone
INFER_OPTIONS =

SOURCES = $(realpath $(wildcard *.swift))

CLEAN_EXTRA = $(foreach source,$(SOURCES),$(basename $(notdir $(source))).o)

include $(TESTS_DIR)/base.make
include $(TESTS_DIR)/swift-base.make

default: test

%.test.sil: %.swift $(SWIFT_DEPS) $(HEADERS)
	$(QUIET)$(call silent_on_success,Testing the infer/swift force-unwrap frontend in $(TEST_REL_DIR),\
	  $(INFER_BIN) capture --results-dir infer-out-$(basename $(notdir $<)) --frontend-tests --dump-textual --project-root $(TESTS_DIR) $(INFER_OPTIONS) -- \
	    $(SWIFTC) $(SWIFT_OPTIONS) $<)

capture: $(SOURCES:.swift=.test.sil)

.PHONY: print
print: capture

.PHONY: test
test: capture
	$(QUIET)error=0; for file in $(SOURCES) ; do \
	  diff -u "$$file.sil" "$$file.test.sil" || error=1 ; \
	  if [ -f "$$file.compiler-generated.sil" ] || [ -f "$$file.compiler-generated.test.sil" ] ; then \
	    diff -u "$$file.compiler-generated.sil" "$$file.compiler-generated.test.sil" || error=1 ; \
	  fi ; \
	done ; \
	if [ $$error = 1 ]; then exit 1; fi

.PHONY: replace
replace: capture
	$(QUIET)for file in $(SOURCES) ; do \
	  mv $$file.test.sil $$file.sil ; \
	  if [ -f "$$file.compiler-generated.test.sil" ] ; then \
	    mv "$$file.compiler-generated.test.sil" "$$file.compiler-generated.sil" ; \
	  fi ; \
	done

.PHONY: clean
clean:
	$(REMOVE_DIR) infer-out infer-out-* *.test.sil $(CLEAN_EXTRA)
