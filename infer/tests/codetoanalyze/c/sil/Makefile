# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR=../../..

SOURCES = $(sort $(wildcard *.c))

include $(TESTS_DIR)/base.make

.NOTPARALLEL:

default: test

# We copy the source file to a temporary directory before capture so that
# --dump-textual writes <name>.sil there instead of overwriting the committed
# .sil file in the source directory.
%.test.sil: %.c
	$(QUIET)$(call silent_on_success,Testing infer/clang textual in $(TEST_REL_DIR),\
	  tmpdir=$$(mktemp -d) && \
	  cp $< $$tmpdir/ && \
	  $(INFER_BIN) capture --dump-textual --project-root $(TESTS_DIR) -- clang -c $$tmpdir/$< && \
	  mv $$tmpdir/$*.sil $*.test.sil && \
	  rm -rf $$tmpdir)

capture: $(SOURCES:.c=.test.sil)

# Verify that the dumped textual can be parsed back (dump-parse roundtrip).
.PHONY: parse
parse: capture
	$(QUIET)error=0; for file in $(SOURCES) ; do \
	  base=$$(basename $$file .c) ; \
	  if $(INFER_BIN) --capture-textual "$$base.test.sil" 2>&1 \
	       | grep -q "syntax error" ; then \
	    echo "PARSE ERROR in $$base.test.sil:" ; \
	    $(INFER_BIN) --capture-textual "$$base.test.sil" 2>&1 \
	      | grep "syntax error" ; \
	    error=1 ; \
	  fi ; \
	done ; \
	if [ $$error = 1 ]; then exit 1; fi

.PHONY: test
test: capture parse
	$(QUIET)error=0; for file in $(SOURCES) ; do \
	  base=$$(basename $$file .c) ; \
	  diff -u "$$base.sil" "$$base.test.sil" || error=1 ; \
	done ; \
	if [ $$error = 1 ]; then exit 1; fi

.PHONY: replace
replace: capture
	$(QUIET)for file in $(SOURCES) ; do \
	  base=$$(basename $$file .c) ; \
	  mv "$$base.test.sil" "$$base.sil" ; \
	done

.PHONY: clean
clean:
	$(REMOVE_DIR) infer-out *.test.sil $(CLEAN_EXTRA)
