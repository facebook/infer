# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

INFER_OPTIONS = --report-custom-error --developer-mode
SOURCES = ../codetoanalyze/path\ with\ spaces/hel\ lo.c
CLEAN_EXTRA = compile_commands.json *.o
INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/infer.make

../codetoanalyze/path\ with\ spaces/hel\ lo.c: ../codetoanalyze/path_with_spaces/hel_lo.c
#	make does not want to interpret "$(@D)" in the right way here...
	$(QUIET)$(MKDIR_P) ../codetoanalyze/path\ with\ spaces/
#	allow copy races with other tests if they are doing the same action
	$(QUIET)$(call copy_or_same_file,$<,$@)
	$(QUIET)touch "$@"

compile_commands.json: compile_commands.json.in
	$(QUIET)sed -e 's#%pwd%#$(CURDIR)#g' $< > $@ || $(REMOVE) $@

infer-out/report.json: compile_commands.json $(SOURCES) Makefile \
                       ../codetoanalyze/path\ with\ spaces/hel\ lo.c $(CLANG_DEPS)
	$(QUIET)$(call silent_on_success,Clang compilation database escaped integration,\
	  $(INFER_BIN) $(INFER_OPTIONS) --project-root $(TESTS_DIR) \
	  --compilation-database-escaped $<)
