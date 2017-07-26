# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..

ANALYZER = infer
INFER_OPTIONS = --report-custom-error --developer-mode --no-keep-going
SOURCES = ../codetoanalyze/path\ with\ spaces/hel\ lo.c
OBJECTS = $(SOURCES:.c=.o)
CLEAN_EXTRA = compile_commands.json
INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/infer.make

../codetoanalyze/path\ with\ spaces/hel\ lo.c: ../codetoanalyze/path_with_spaces/hel_lo.c
#	make does not want to interpret "$(@D)" in the right way here...
	$(QUIET)$(MKDIR_P) ../codetoanalyze/path\ with\ spaces/
	$(QUIET)cp "$<" "$@"
	$(QUIET)touch "$@"

compile_commands.json: compile_commands.json.in
	$(QUIET)sed -e 's#%pwd%#$(CURDIR)#g' $< > $@ || $(REMOVE) $@

infer-out/report.json: compile_commands.json $(SOURCES) Makefile \
                       ../codetoanalyze/path\ with\ spaces/hel\ lo.c
	$(QUIET)$(call silent_on_success,Clang compilation database escaped integration,\
	  $(INFER_BIN) -a $(ANALYZER) $(INFER_OPTIONS) --project-root $(TESTS_DIR) \
	  --compilation-database-escaped $<)
