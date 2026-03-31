# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Shared rules for --store-textual / --export-textual tests.
#
# Each including Makefile must define:
#   TESTS_DIR  — relative path to infer/tests (usually ../../..)
#   SOURCES    — list of source files to capture
#   capture    — a phony target that runs infer capture --store-textual
#
# Optional:
#   PARSE_EXPORTED — set to "no" to skip parsing exported .sil files back
#   MANIFEST_FILTER — optional command to filter manifest.json (e.g. to strip model entries)

include $(TESTS_DIR)/base.make

.NOTPARALLEL:

default: test

EXPORT_DIR = export-out
PARSE_EXPORTED ?= yes
MANIFEST_FILTER ?= cat

.PHONY: export
export: capture
	$(QUIET)$(call silent_on_success,Exporting textual in $(TEST_REL_DIR),\
	  $(INFER_BIN) debug --export-textual $(EXPORT_DIR))

manifest.test: export
	$(QUIET)$(MANIFEST_FILTER) < $(EXPORT_DIR)/manifest.json > manifest.test

# Verify that exported textual files exist, are non-empty, and can be parsed back.
.PHONY: parse
parse: export
ifeq ($(PARSE_EXPORTED),yes)
	$(QUIET)error=0; for sil in $(EXPORT_DIR)/*.sil ; do \
	  if [ ! -s "$$sil" ]; then \
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
endif

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
