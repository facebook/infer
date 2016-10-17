# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

ROOT_DIR = .
-include $(ROOT_DIR)/Makefile.config

ifeq ($(IS_FACEBOOK_TREE),yes)
# With this makefile, all targets will default to have right env variables
# pointing to the sandbox
	-include $(ROOT_DIR)/facebook//Makefile.env
endif

DIRECT_TESTS=
ifeq ($(BUILD_C_ANALYZERS),yes)
DIRECT_TESTS += c_infer_test c_frontend_test cpp_infer_test cpp_frontend_test cpp_quandary_test
endif
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
DIRECT_TESTS += \
  java_checkers_test java_eradicate_test java_infer_test java_tracing_test \
  java_quandary_test java_crashcontext_test java_harness_test
endif
ifneq ($(XCODE_SELECT),no)
DIRECT_TESTS += objc_frontend_test objc_infer_test objc_linters objcpp_frontend_test objcpp_linters
endif

all: infer inferTraceBugs

$(INFERTRACEBUGS_BIN_RELPATH):
	($(REMOVE) $@ && \
	 cd $(@D) && \
	 $(LN_S) ../lib/python/$(@F) $(@F))

src_build:
ifeq ($(IS_FACEBOOK_TREE),yes)
	$(MAKE) -C facebook
endif
	$(MAKE) -C $(SRC_DIR) infer
ifeq ($(BUILD_C_ANALYZERS),yes)
src_build: clang_plugin
endif

infer: src_build
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
	$(MAKE) -C $(ANNOTATIONS_DIR)
endif
	$(MAKE) -C $(MODELS_DIR) all

clang_setup:
	export CC="$(CC)" CFLAGS="$(CFLAGS)"; \
	export CXX="$(CXX)" CXXFLAGS="$(CXXFLAGS)"; \
	export CPP="$(CPP)" LDFLAGS="$(LDFLAGS)" LIBS="$(LIBS)"; \
	$(FCP_DIR)/clang/setup.sh $(INFER_FCP_SETUP_OPTS)

clang_plugin: clang_setup
ifeq ($(IS_RELEASE_TREE),no)
	$(MAKE) -C $(FCP_DIR)/libtooling all \
	  CC=$(CC) CXX=$(CXX) \
	  CFLAGS="$(CFLAGS)" CXXFLAGS="$(CXXFLAGS)" \
	  CPP="$(CPP)" LDFLAGS="$(LDFLAGS)" LIBS="$(LIBS)" \
	  LOCAL_CLANG=$(CLANG_PREFIX)/bin/clang \
	  CLANG_PREFIX=$(CLANG_PREFIX) \
	  CLANG_INCLUDES=$(CLANG_INCLUDES)
	$(MAKE) -C $(FCP_DIR)/clang-ocaml all \
          build/clang_ast_proj.ml build/clang_ast_proj.mli \
	  CC=$(CC) CXX=$(CXX) \
	  CFLAGS="$(CFLAGS)" CXXFLAGS="$(CXXFLAGS)" \
	  CPP="$(CPP)" LDFLAGS="$(LDFLAGS)" LIBS="$(LIBS)" \
	  LOCAL_CLANG=$(CLANG_PREFIX)/bin/clang \
	  CLANG_PREFIX=$(CLANG_PREFIX) \
	  CLANG_INCLUDES=$(CLANG_INCLUDES)
endif

inferTraceBugs: $(INFERTRACEBUGS_BIN_RELPATH)

test_this_build: clang_plugin
ifeq ($(IS_FACEBOOK_TREE),yes)
	$(MAKE) -C facebook
endif
	$(MAKE) -C $(SRC_DIR) test_build

test_oss_build: clang_plugin
# make sure we don't break the opensource build
ifeq ($(IS_FACEBOOK_TREE),yes)
	$(MAKE) -C facebook clean
endif
	$(MAKE) -C $(SRC_DIR) EXTRA_DEPS=opensource TEST_BUILD_DIR=$(BUILD_DIR)/opensource test_build

test_build: test_this_build
ifeq ($(IS_FACEBOOK_TREE),yes)
test_build: test_oss_build
endif

ocaml_unit_test: test_this_build
	$(TEST_BUILD_DIR)/unit/inferunit.byte

frontend_replace: c_frontend_replace cpp_frontend_replace objc_frontend_replace objcpp_frontend_replace

c_frontend_replace:
	$(MAKE) -C ./infer/tests/codetoanalyze/c/frontend replace

c_frontend_test:
	$(MAKE) -C ./infer/tests/codetoanalyze/c/frontend test

c_infer_test:
	$(MAKE) -C ./infer/tests/codetoanalyze/c/errors test

cpp_frontend_replace:
	$(MAKE) -C ./infer/tests/codetoanalyze/cpp/frontend replace

cpp_frontend_test:
	$(MAKE) -C ./infer/tests/codetoanalyze/cpp/frontend test

cpp_infer_test:
	$(MAKE) -C ./infer/tests/codetoanalyze/cpp/errors test

cpp_quandary_test:
	make -C ./infer/tests/codetoanalyze/cpp/quandary test

java_checkers_test:
	$(MAKE) -C ./infer/tests/codetoanalyze/java/checkers test

java_crashcontext_test:
	$(MAKE) -C ./infer/tests/codetoanalyze/java/crashcontext test

java_eradicate_test:
	$(MAKE) -C ./infer/tests/codetoanalyze/java/eradicate test

java_harness_test:
	$(MAKE) -C ./infer/tests/codetoanalyze/java/harness test

java_infer_test:
	$(MAKE) -C ./infer/tests/codetoanalyze/java/infer test

java_tracing_test:
	$(MAKE) -C ./infer/tests/codetoanalyze/java/tracing test

java_quandary_test:
	$(MAKE) -C ./infer/tests/codetoanalyze/java/quandary test

objc_frontend_replace:
	$(MAKE) -C ./infer/tests/codetoanalyze/objc/frontend replace

objc_frontend_test:
	$(MAKE) -C ./infer/tests/codetoanalyze/objc/frontend test

objc_infer_test:
	$(MAKE) -C ./infer/tests/codetoanalyze/objc/errors test

objc_linters:
	$(MAKE) -C ./infer/tests/codetoanalyze/objc/linters test

objcpp_frontend_replace:
	$(MAKE) -C ./infer/tests/codetoanalyze/objcpp/frontend replace

objcpp_frontend_test:
	$(MAKE) -C ./infer/tests/codetoanalyze/objcpp/frontend test

objcpp_linters:
	$(MAKE) -C ./infer/tests/codetoanalyze/objcpp/linters test

direct_tests:
	$(MAKE) -j $(NCPU) -l $(NCPU) $(DIRECT_TESTS)

buck_test: infer
	$(MAKE) direct_tests
	NO_BUCKD=1 ./infer/tests/build_systems/build_integration_tests.py

buck_test_xml: infer
	$(MAKE) direct_tests
	buck test --xml test.xml # TODO: generate test.xml with test results
	NO_BUCKD=1 ./infer/tests/build_systems/build_integration_tests.py

inferTraceBugs_test: infer
	$(INFER_BIN) -o __test-infer-out__ -- \
	  javac $(EXAMPLES_DIR)/Hello.java \
	   > /dev/null
	@rm -f Hello.class
	$(PYTHON_DIR)/inferTraceBugs -o __test-infer-out__ \
	  --select 0 --max-level max > /dev/null
	$(PYTHON_DIR)/inferTraceBugs -o __test-infer-out__ \
	  --select 0 --max-level 0 > /dev/null
	$(PYTHON_DIR)/inferTraceBugs -o __test-infer-out__ \
	  --select 0 --max-level max --no-source > /dev/null
	$(PYTHON_DIR)/inferTraceBugs -o __test-infer-out__ \
	  --only-show > /dev/null
	@rm -fr __test-infer-out__

check_missing_mli:
	@bash -c '\
	for x in `find infer/src -name "*.ml" -or  -name "*.re"`; do \
		test -f "$$x"i || echo Missing "$$x"i; done'

inferScriptMode_test: toplevel
	INFER_REPL_BINARY=ocaml ./scripts/infer_repl ./infer/tests/repl/infer_batch_script.ml

test: test_build ocaml_unit_test buck_test inferTraceBugs_test inferScriptMode_test
	$(MAKE) -C $(SRC_DIR) mod_dep.dot

test_xml: test_build ocaml_unit_test buck_test_xml inferTraceBugs_test
	$(MAKE) -C $(SRC_DIR) mod_dep.dot

quick-test: test_this_build ocaml_unit_test

toplevel:
	$(MAKE) -C $(SRC_DIR) toplevel

uninstall:
	$(REMOVE_DIR) $(DESTDIR)$(libdir)/infer/
	$(REMOVE) $(DESTDIR)$(bindir)/inferTraceBugs
	$(REMOVE) $(DESTDIR)$(bindir)/infer

install: infer inferTraceBugs
# create directory structure
	test -d      $(DESTDIR)$(bindir) || \
	  $(MKDIR_P) $(DESTDIR)$(bindir)
	test -d      $(DESTDIR)$(libdir)/infer/ || \
	  $(MKDIR_P) $(DESTDIR)$(libdir)/infer/
ifeq ($(BUILD_C_ANALYZERS),yes)
	test -d      $(DESTDIR)$(libdir)/infer/facebook-clang-plugins/libtooling/build/ || \
	  $(MKDIR_P) $(DESTDIR)$(libdir)/infer/facebook-clang-plugins/libtooling/build/
	@for i in $$(find facebook-clang-plugins/clang/install -type d); do \
	  test -d      $(DESTDIR)$(libdir)/infer/$$i || \
	    $(MKDIR_P) $(DESTDIR)$(libdir)/infer/$$i; \
	done
	test -d      $(DESTDIR)$(libdir)/infer/infer/lib/clang_wrappers/ || \
	  $(MKDIR_P) $(DESTDIR)$(libdir)/infer/infer/lib/clang_wrappers/
	@for i in $$(find infer/models/cpp/include/ -type d); do \
	  test -d      $(DESTDIR)$(libdir)/infer/$$i || \
	    $(MKDIR_P) $(DESTDIR)$(libdir)/infer/$$i; \
	done
endif
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
	test -d      $(DESTDIR)$(libdir)/infer/infer/lib/java/ || \
	  $(MKDIR_P) $(DESTDIR)$(libdir)/infer/infer/lib/java/
endif
ifneq ($(XCODE_SELECT),no)
	test -d      $(DESTDIR)$(libdir)/infer/infer/lib/xcode_wrappers/ || \
	  $(MKDIR_P) $(DESTDIR)$(libdir)/infer/infer/lib/xcode_wrappers/
endif
	test -d      $(DESTDIR)$(libdir)/infer/infer/annotations/ || \
	  $(MKDIR_P) $(DESTDIR)$(libdir)/infer/infer/annotations/
	test -d      $(DESTDIR)$(libdir)/infer/infer/lib/wrappers/ || \
	  $(MKDIR_P) $(DESTDIR)$(libdir)/infer/infer/lib/wrappers/
	test -d      $(DESTDIR)$(libdir)/infer/infer/lib/specs/ || \
	  $(MKDIR_P) $(DESTDIR)$(libdir)/infer/infer/lib/specs/
	test -d      $(DESTDIR)$(libdir)/infer/infer/lib/python/ || \
	  $(MKDIR_P) $(DESTDIR)$(libdir)/infer/infer/lib/python/
	test -d      $(DESTDIR)$(libdir)/infer/infer/lib/python/inferlib/ || \
	  $(MKDIR_P) $(DESTDIR)$(libdir)/infer/infer/lib/python/inferlib/
	test -d      $(DESTDIR)$(libdir)/infer/infer/lib/python/inferlib/capture/ || \
	  $(MKDIR_P) $(DESTDIR)$(libdir)/infer/infer/lib/python/inferlib/capture/
	test -d      $(DESTDIR)$(libdir)/infer/infer/bin/ || \
	  $(MKDIR_P) $(DESTDIR)$(libdir)/infer/infer/bin/

# copy files
ifeq ($(BUILD_C_ANALYZERS),yes)
	$(INSTALL_DATA) -C          facebook-clang-plugins/libtooling/build/FacebookClangPlugin.dylib \
	  $(DESTDIR)$(libdir)/infer/facebook-clang-plugins/libtooling/build/FacebookClangPlugin.dylib
	@for i in $$(find facebook-clang-plugins/clang/install -not -type d); do \
	  $(INSTALL_PROGRAM) -C $$i $(DESTDIR)$(libdir)/infer/$$i; \
	done
	@for i in $$(find infer/lib/clang_wrappers/*); do \
	  $(INSTALL_PROGRAM) -C $$i $(DESTDIR)$(libdir)/infer/$$i; \
	done
	@for i in $$(find infer/lib/specs/*); do \
	  $(INSTALL_DATA) -C $$i $(DESTDIR)$(libdir)/infer/$$i; \
	done
	@for i in $$(find infer/models/cpp/include/ -not -type d); do \
		$(INSTALL_DATA) -C $$i $(DESTDIR)$(libdir)/infer/$$i; \
	done
	$(INSTALL_PROGRAM) -C $(INFERCLANG_BIN) $(DESTDIR)$(libdir)/infer/infer/bin/
	$(INSTALL_PROGRAM) -C $(INFER_BUCK_COMPILATION_DATABASE_BIN) $(DESTDIR)$(libdir)/infer/infer/bin/
endif
ifneq ($(XCODE_SELECT),no)
	@for i in $$(find infer/lib/xcode_wrappers/*); do \
	  $(INSTALL_PROGRAM) -C $$i $(DESTDIR)$(libdir)/infer/$$i; \
	done
endif
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
	$(INSTALL_DATA) -C          infer/annotations/annotations.jar \
	  $(DESTDIR)$(libdir)/infer/infer/annotations/annotations.jar
	@for i in infer/lib/java/*.jar; do \
	  $(INSTALL_DATA) -C $$i $(DESTDIR)$(libdir)/infer/$$i; \
	done
	$(INSTALL_PROGRAM) -C $(INFERJAVA_BIN) $(DESTDIR)$(libdir)/infer/infer/bin/
endif
	@for i in $$(find infer/lib/wrappers/*); do \
	  $(INSTALL_PROGRAM) -C $$i $(DESTDIR)$(libdir)/infer/$$i; \
	done
	@for i in $$(find infer/lib/python/inferlib/* -type f); do \
	  $(INSTALL_DATA) -C $$i $(DESTDIR)$(libdir)/infer/$$i; \
	done
	$(INSTALL_PROGRAM) -C       infer/lib/python/infer.py \
	  $(DESTDIR)$(libdir)/infer/infer/lib/python/infer.py
	$(INSTALL_PROGRAM) -C       infer/lib/python/inferTraceBugs \
	  $(DESTDIR)$(libdir)/infer/infer/lib/python/inferTraceBugs
	$(INSTALL_PROGRAM) -C $(INFER_BIN) $(DESTDIR)$(libdir)/infer/infer/bin/
	$(INSTALL_PROGRAM) -C $(INFERANALYZE_BIN) $(DESTDIR)$(libdir)/infer/infer/bin/
	$(INSTALL_PROGRAM) -C $(INFERPRINT_BIN) $(DESTDIR)$(libdir)/infer/infer/bin/
	$(INSTALL_PROGRAM) -C $(INFERSTATS_BIN) $(DESTDIR)$(libdir)/infer/infer/bin/
	(cd $(DESTDIR)$(bindir)/ && \
	 $(REMOVE) infer && \
	 $(LN_S) $(libdir)/infer/infer/bin/infer infer)
	(cd $(DESTDIR)$(bindir)/ && \
	 $(REMOVE) inferTraceBugs && \
	 $(LN_S) $(libdir)/infer/infer/lib/python/inferTraceBugs inferTraceBugs)

ifeq ($(IS_FACEBOOK_TREE),yes)
	$(MAKE) -C facebook install
endif

clean:
	$(REMOVE) test.xml
ifeq ($(IS_RELEASE_TREE),no)
ifeq ($(BUILD_C_ANALYZERS),yes)
	$(MAKE) -C $(FCP_DIR) clean
	$(MAKE) -C $(FCP_DIR)/clang-ocaml clean
endif
endif
	$(MAKE) -C $(SRC_DIR) clean
	$(MAKE) -C $(ANNOTATIONS_DIR) clean
	$(MAKE) -C $(MODELS_DIR) clean
	$(REMOVE) $(INFERTRACEBUGS_BIN_RELPATH)
ifeq ($(IS_FACEBOOK_TREE),yes)
	$(MAKE) -C facebook clean
endif
	$(MAKE) -C $(DEPENDENCIES_DIR)/ocamldot clean

conf-clean: clean
	$(REMOVE) infer/lib/python/inferlib/*.pyc
	$(REMOVE) infer/lib/python/inferlib/*/*.pyc
	$(REMOVE) .buckversion
	$(REMOVE) Makefile.config
	$(REMOVE) acinclude.m4
	$(REMOVE) aclocal.m4
	$(REMOVE_DIR) autom4te.cache/
	$(REMOVE) config.log
	$(REMOVE) config.status
	$(REMOVE) configure
	$(REMOVE_DIR) infer/models/c/out/
	$(REMOVE_DIR) infer/models/cpp/out/
	$(REMOVE_DIR) infer/models/java/infer-out/
	$(REMOVE_DIR) infer/models/objc/out/

.PHONY: all buck_test buck_test_xml clean clang_plugin clang_setup infer inferTraceBugs
.PHONY: inferTraceBugs_test install ocaml_unit_test check_missing_mli src_build test test_xml
.PHONY: test_build toplevel uninstall

# print any variable for Makefile debugging
print-%:
	@echo '$*=$($*)'
