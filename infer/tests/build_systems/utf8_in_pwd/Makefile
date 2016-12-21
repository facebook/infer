# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

# This is mostly a copy-paste-adapt of other integration tests with the twist that the code to
# analyze in placed in a path that contains utf8 characters. This does not provide a 'compile'
# or 'analyze' target.

TESTS_DIR = ../..
ROOT_DIR = $(TESTS_DIR)/../..

include $(TESTS_DIR)/java.make
include $(ROOT_DIR)/Makefile.config

# see base.make
TEST_REL_DIR = $(patsubst $(abspath $(TESTS_DIR))/%,%,$(abspath $(CURDIR)))

UTF8_DIR = $(shell printf '../codetoanalyze/utf8_\u03B9n_pwd')

CMAKE_DIR = $(UTF8_DIR)/cmake
CMAKE_BUILD_DIR = $(CMAKE_DIR)/_build
CMAKE_ANALYZE_DIR = $(CMAKE_DIR)/_build_infer
CMAKE_SOURCES_DIR = ../codetoanalyze/cmake
CMAKE_SOURCES = $(CMAKE_SOURCES_DIR)/hello.c

GRADLE_DIR = $(UTF8_DIR)/gradle
GRADLE_SOURCES_DIR = $(ROOT_DIR)/examples/java_hello
GRADLE_SOURCES = $(wildcard $(GRADLE_SOURCES_DIR)/*.java)

JAVAC_DIR = $(UTF8_DIR)/javac
JAVAC_SOURCES = $(ROOT_DIR)/examples/Hello.java

MAKE_DIR = $(UTF8_DIR)/make
MAKE_SOURCES_DIR = ../codetoanalyze/make
MAKE_SOURCES = $(wildcard $(MAKE_SOURCES_DIR)/*.c)

ANALYZER = infer
CLEAN_EXTRA = $(CMAKE_BUILD_DIR) $(CMAKE_ANALYZE_DIR) infer-out-cmake
INFERPRINT_OPTIONS = --issues-tests

default: print

$(UTF8_DIR):
	$(MKDIR_P) $@

$(CMAKE_DIR): $(CMAKE_SOURCES) $(UTF8_DIR)
	rsync -aL $(CMAKE_SOURCES_DIR)/ $@/
	touch $@

$(GRADLE_DIR): $(GRADLE_SOURCES) $(UTF8_DIR)
	rsync -a $(GRADLE_SOURCES_DIR)/ $@/
	touch $@

$(JAVAC_DIR): $(JAVAC_SOURCES) $(UTF8_DIR)
	$(MKDIR_P) $(JAVAC_DIR)
	rsync -a $(JAVAC_SOURCES) $@
	touch $@

$(MAKE_DIR): $(MAKE_SOURCES) $(UTF8_DIR)
	rsync -a $(MAKE_SOURCES_DIR)/ $@/
	touch $@

infer-out-cmake/report.json: $(CMAKE_DIR) $(CMAKE_SOURCES_DIR)/CMakeLists.txt $(CLANG_DEPS)
	$(REMOVE_DIR) $(CMAKE_ANALYZE_DIR) && \
	$(MKDIR_P) $(CMAKE_ANALYZE_DIR) && \
	cd $(CMAKE_ANALYZE_DIR) && \
	$(call silent_on_success,\
	  $(INFER_BIN) -a compile --project-root $(CMAKE_DIR) --results-dir $(CURDIR)/$(@D) -- cmake ..)
	cd $(CMAKE_ANALYZE_DIR) && \
	$(call silent_on_success,\
	  $(INFER_BIN) -a $(ANALYZER) --results-dir $(CURDIR)/$(@D) -- $(MAKE))
	sed -i -e 's#$(abspath $(CMAKE_DIR))/##g' $@

infer-out-gradle/report.json: $(JAVA_DEPS) $(GRADLE_DIR)
#	mock version of gradle
	PATH=$(CURDIR)/../mock:"$$PATH"; \
	cd $(GRADLE_DIR) && \
	$(call silent_on_success,\
	  $(INFER_BIN) -a $(ANALYZER) --results-dir $(CURDIR)/$(@D) -- gradle build)

infer-out-javac/report.json: $(JAVA_DEPS) $(JAVAC_DIR)
	cd $(JAVAC_DIR) && \
	$(call silent_on_success,\
	  $(INFER_BIN) -a $(ANALYZER) --results-dir $(CURDIR)/$(@D) -- \
	    javac -cp $(CLASSPATH) $(foreach source,$(JAVAC_SOURCES),$(notdir $(source))))

infer-out-make/report.json: $(CLANG_DEPS) $(MAKE_DIR)
	cd $(MAKE_DIR) && \
	$(call silent_on_success,\
	  $(INFER_BIN) -a $(ANALYZER) --results-dir $(CURDIR)/$(@D) -- \
	    make clean all)

issues-%.exp.test.noreplace: infer-out-%/report.json $(INFERPRINT_BIN)
	$(INFERPRINT_BIN) -q -a $(ANALYZER) $(INFERPRINT_OPTIONS) $@ --from-json-report $<

issues.exp.test: issues-cmake.exp.test.noreplace issues-gradle.exp.test.noreplace \
                 issues-javac.exp.test.noreplace issues-make.exp.test.noreplace
	cat $^ > $@

.PHONY: print
print: issues.exp.test

.PHONY: test
test: issues.exp.test
	@cd $(TESTS_DIR) && \
	diff -u $(TEST_REL_DIR)/issues.exp $(TEST_REL_DIR)/issues.exp.test

.PHONY: clean
clean:
	rm -rf $(UTF8_DIR) issues.exp.test* infer-out-*
