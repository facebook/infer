# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

INFERPRINT_OPTIONS = --issues-tests
TEST_CLASSPATH = $(JAVA_BUILTINS_DIR)

MVN_DIRS = app_with_submodules  simple_app  app_with_infer_profile  app_with_profiles
MVN_ROOT = ../codetoanalyze/mvn
SOURCES = $(shell find $(MVN_ROOT) -type f -name '*.java' -o -name 'pom.xml')

CLEAN_EXTRA = \
  $(MVN_DIRS:%=../codetoanalyze/mvn/%/target) \
  $(MVN_DIRS:%=../codetoanalyze/mvn/%/module1/target) \
  $(MVN_DIRS:%=../codetoanalyze/mvn/%/module2parent/module2/target) \
  $(MVN_DIRS:%=../codetoanalyze/mvn/%/com) \
  $(MVN_DIRS:%=infer-out-%) \
  $(MVN_DIRS:%=issues-%.exp.test)

include $(TESTS_DIR)/java.make
include $(TESTS_DIR)/infer.make

infer-out/report.json:
	$(QUIET)$(MKDIR_P) $(@D)
	$(QUIET)touch $@

infer-out-%/report.json: $(JAVA_DEPS) $(SOURCES)
	$(QUIET)cd $(MVN_ROOT)/$* && \
	$(call silent_on_success,Testing mvn Java integration: $*,\
	  $(INFER_BIN) --pulse-only --results-dir $(CURDIR)/$(@D) \
	    --project-root $(CURDIR)/$(TESTS_DIR) -- \
	    $(MVN) clean compile)

infer-out-app_with_submodules/report.json: infer-out-simple_app/report.json

infer-out-simple_app/report.json: infer-out-app_with_infer_profile/report.json

infer-out-app_with_infer_profile/report.json: infer-out-app_with_profiles/report.json

issues-%.exp.test: infer-out-%/report.json
	$(QUIET)$(INFER_BIN) report -q $(INFERPRINT_OPTIONS) $@ -o $(<D)

issues.exp.test: $(foreach mvndir,$(MVN_DIRS),issues-$(mvndir).exp.test)
#	erase the contents of the file
	$(QUIET): > $@
#	remember the file name so it's easier to know which bug is from where
	$(QUIET)for mvndir in $(MVN_DIRS); do \
	  echo "-- $$mvndir" >> $@; \
	  cat issues-$$mvndir.exp.test >> $@; \
	done
