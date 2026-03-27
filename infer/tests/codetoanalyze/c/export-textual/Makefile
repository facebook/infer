# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR=../../..

SOURCES = $(sort $(wildcard *.c) $(wildcard */*.c))

include $(TESTS_DIR)/base.make

.NOTPARALLEL:

default: test

EXPORT_DIR = export-out

.PHONY: capture
capture:
	$(QUIET)$(call silent_on_success,Capturing with --store-textual in $(TEST_REL_DIR),\
	  $(REMOVE_DIR) infer-out $(EXPORT_DIR) && \
	  $(foreach src,$(SOURCES),\
	    $(INFER_BIN) capture --store-textual --reactive \
	      --project-root $(TESTS_DIR) -- clang -c $(src) -o $(src:.c=.o) &&) \
	  true)

.PHONY: export
export: capture
	$(QUIET)$(call silent_on_success,Exporting textual in $(TEST_REL_DIR),\
	  $(INFER_BIN) debug --export-textual $(EXPORT_DIR))

manifest.test: export
	$(QUIET)cp $(EXPORT_DIR)/manifest.json manifest.test

# Verify that exported textual files exist, are non-empty, and can be parsed back.
.PHONY: parse
parse: export
	$(QUIET)error=0; for src in $(SOURCES) ; do \
	  sil=$(EXPORT_DIR)/$$(basename $$src .c).sil ; \
	  if [ ! -f "$$sil" ]; then \
	    echo "MISSING: $$sil" ; error=1 ; \
	  elif [ ! -s "$$sil" ]; then \
	    echo "EMPTY: $$sil" ; error=1 ; \
	  elif $(INFER_BIN) --capture-textual "$$sil" 2>&1 \
	       | grep -q "syntax error" ; then \
	    echo "PARSE ERROR in $$sil:" ; \
	    $(INFER_BIN) --capture-textual "$$sil" 2>&1 \
	      | grep "syntax error" ; \
	    error=1 ; \
	  fi ; \
	done ; \
	if [ $$error = 1 ]; then exit 1; fi

.PHONY: test
test: manifest.test parse
	$(QUIET)diff -u manifest.exp manifest.test || \
	  (printf '$(TERM_ERROR)manifest mismatch$(TERM_RESET)\n' >&2; exit 1)
	$(QUIET)$(REMOVE) manifest.test
	$(QUIET)$(REMOVE_DIR) $(EXPORT_DIR)

.PHONY: replace
replace: manifest.test
	$(QUIET)cp manifest.test manifest.exp
	$(QUIET)$(REMOVE) manifest.test
	$(QUIET)$(REMOVE_DIR) $(EXPORT_DIR)

.PHONY: clean
clean:
	$(REMOVE_DIR) infer-out $(EXPORT_DIR) *.test *.o $(CLEAN_EXTRA)
