# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# This is mostly a copy-paste-adapt of other integration tests with the twist that the code to
# analyze in placed in a path that contains utf8 characters. This does not provide a 'compile'
# or 'analyze' target.

TESTS_DIR = ../..
TEST_CLASSPATH = $(JAVA_BUILTINS_DIR)

include $(TESTS_DIR)/java.make
include $(TESTS_DIR)/base.make

UTF8_DIR = $(shell printf '../codetoanalyze/utf8_\u03B9n_pwd')

CMAKE_DIR = $(UTF8_DIR)/cmake
CMAKE_BUILD_DIR = $(CMAKE_DIR)/_build
CMAKE_ANALYZE_DIR = $(CMAKE_DIR)/_build_infer
CMAKE_SOURCES_DIR = ../codetoanalyze/cmake
CMAKE_SOURCES = $(CMAKE_SOURCES_DIR)/hello.c
CMAKE_FILES = $(CMAKE_SOURCES) $(CMAKE_SOURCES_DIR)/CMakeLists.txt

GRADLE_DIR = $(UTF8_DIR)/gradle
GRADLE_SOURCES_DIR = $(ROOT_DIR)/examples/java_hello
GRADLE_SOURCES = $(wildcard $(GRADLE_SOURCES_DIR)/*.java)
GRADLE_FILES = $(GRADLE_SOURCES)

JAVAC_DIR = $(UTF8_DIR)/javac
JAVAC_SOURCES = $(ROOT_DIR)/examples/Hello.java
JAVAC_FILES = $(JAVAC_SOURCES)

MAKE_DIR = $(UTF8_DIR)/make
MAKE_SOURCES_DIR = ../codetoanalyze/make
MAKE_SOURCES = $(wildcard $(MAKE_SOURCES_DIR)/*.c $(MAKE_SOURCES_DIR)/*.java)
MAKE_FILES = $(MAKE_SOURCES) $(MAKE_SOURCES_DIR)/Makefile

CLEAN_EXTRA = $(CMAKE_BUILD_DIR) $(CMAKE_ANALYZE_DIR) infer-out-cmake
INFERPRINT_OPTIONS = --issues-tests

default: print

$(UTF8_DIR):
	$(QUIET)$(MKDIR_P) $@

$(CMAKE_DIR): $(CMAKE_SOURCES) $(UTF8_DIR)
	$(QUIET)$(MKDIR_P) $@
	$(QUIET)cp $(CMAKE_FILES) $@/
	$(QUIET)touch $@

$(GRADLE_DIR): $(GRADLE_SOURCES) $(UTF8_DIR)
	$(QUIET)$(MKDIR_P) $@
	$(QUIET)cp $(GRADLE_FILES) $@/
	$(QUIET)touch $@

$(JAVAC_DIR): $(JAVAC_SOURCES) $(UTF8_DIR)
	$(QUIET)$(MKDIR_P) $@
	$(QUIET)cp $(JAVAC_FILES) $@/
	$(QUIET)touch $@

$(MAKE_DIR): $(MAKE_SOURCES) $(UTF8_DIR)
	$(QUIET)$(MKDIR_P) $@
	$(QUIET)rsync -a $(MAKE_FILES) $@/
	$(QUIET)touch $@

infer-out-cmake/report.json: $(CMAKE_DIR) $(CMAKE_SOURCES_DIR)/CMakeLists.txt $(CLANG_DEPS)
	$(QUIET)$(REMOVE_DIR) $(CMAKE_ANALYZE_DIR)
	$(QUIET)$(MKDIR_P) $(CMAKE_ANALYZE_DIR)
	$(QUIET)cd $(CMAKE_ANALYZE_DIR) && \
	$(call silent_on_success,Testing\
	  Infer/CMake is immune to UTF-8 in PWD: Makefile generation,\
	  $(INFER_BIN) compile --project-root .. --results-dir $(CURDIR)/$(@D) -- $(CMAKE) ..)
	$(QUIET)cd $(CMAKE_ANALYZE_DIR) && \
	$(call silent_on_success,Testing\
	  Infer/CMake is immune to UTF-8 in PWD: Makefile execution,\
	  $(INFER_BIN) --results-dir $(CURDIR)/$(@D) -- make)
	$(QUIET)sed -i -e 's#$(abspath $(CMAKE_DIR))/##g' $@

infer-out-gradle/report.json: $(JAVA_DEPS) $(GRADLE_DIR)
#	mock version of gradle
	$(QUIET)PATH=$(CURDIR)/../mock:"$$PATH"; \
	cd $(GRADLE_DIR) && \
	$(call silent_on_success,Testing Infer/gradle is immune to UTF-8 in PWD,\
	  $(INFER_BIN) --results-dir $(CURDIR)/$(@D) -- gradle build)

infer-out-javac/report.json: $(JAVA_DEPS) $(JAVAC_DIR)
	$(QUIET)cd $(JAVAC_DIR) && \
	$(call silent_on_success,Testing Infer/javac is immune to UTF-8 in PWD,\
	  $(INFER_BIN) --results-dir $(CURDIR)/$(@D) -- \
	    $(JAVAC) -cp $(CLASSPATH) $(foreach source,$(JAVAC_SOURCES),$(notdir $(source))))

infer-out-make/report.json: $(CLANG_DEPS) $(MAKE_DIR)
	$(QUIET)cd $(MAKE_DIR) && \
	$(call silent_on_success,Testing Infer/make is immune to UTF-8 in PWD,\
	  $(TESTLOCK) sh -c "$(MAKE) clean && $(INFER_BIN) --results-dir $(CURDIR)/$(@D) -- $(MAKE) all")

issues-%.exp.test: infer-out-%/report.json $(INFER_BIN)
	$(QUIET)$(INFER_BIN) report -q $(INFERPRINT_OPTIONS) $@ -o $(<D)

issues.exp.test: issues-cmake.exp.test issues-gradle.exp.test issues-javac.exp.test \
                 issues-make.exp.test
	$(QUIET)cat $^ > $@

.PHONY: print
print: issues.exp.test

.PHONY: test
test: issues.exp.test
	$(QUIET)cd $(TESTS_DIR) && \
	$(call check_no_diff,$(TEST_REL_DIR)/issues.exp,$(TEST_REL_DIR)/issues.exp.test)

.PHONY: replace
replace: issues.exp.test
	cp $< issues.exp

.PHONY: clean
clean:
	rm -rf $(UTF8_DIR) issues*.exp.test* infer-out-*
