# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

# the sources do not compile on purpose; with --keep-going infer analyzes them anyway
INFER_OPTIONS = --keep-going --no-default-checkers --liveness --pulse
SOURCES = ../codetoanalyze/clang_compile_errors/undeduced_auto.cpp
CLEAN_EXTRA = compile_commands.json
INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/infer.make

compile_commands.json: compile_commands.json.in
	$(QUIET)sed -e 's#%pwd%#$(CURDIR)#g' $< > $@ || $(REMOVE) $@

infer-out/report.json: compile_commands.json $(SOURCES) $(MAKEFILE_LIST) $(CLANG_DEPS)
	$(QUIET)$(call silent_on_success,Clang compilation database with compile errors,\
	  $(INFER_BIN) $(INFER_OPTIONS) --project-root $(TESTS_DIR) \
	  --compilation-database $<)
