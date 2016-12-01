# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

ROOT_DIR = .
include $(ROOT_DIR)/Makefile.config

ifeq ($(IS_FACEBOOK_TREE),yes)
# With this makefile, all targets will default to have right env variables pointing to the sandbox
  include $(ROOT_DIR)/facebook/Makefile.env
endif

BUILD_SYSTEMS_TESTS = assembly clang_translation linters project_root_rel
ifneq ($(ANT),no)
BUILD_SYSTEMS_TESTS += ant
endif
ifneq ($(BUCK),no)
BUILD_SYSTEMS_TESTS += buck
endif
ifneq ($(CMAKE),no)
BUILD_SYSTEMS_TESTS += clang_compilation_db
endif

DIRECT_TESTS=
ifeq ($(BUILD_C_ANALYZERS),yes)
DIRECT_TESTS += c_errors_test c_frontend_test cpp_checkers_test cpp_errors_test cpp_frontend_test cpp_quandary_test
endif
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
DIRECT_TESTS += \
  java_checkers_test java_eradicate_test java_infer_test java_tracing_test \
  java_quandary_test java_threadsafety_test java_crashcontext_test java_harness_test
endif
ifneq ($(XCODE_SELECT),no)
DIRECT_TESTS += objc_frontend_test objc_errors_test objc_linters_test objcpp_frontend_test objcpp_linters_test
endif

.PHONY: all
all: infer inferTraceBugs

# executable names that should point to InferClang to trigger capture
INFERCLANG_WRAPPERS_BASENAMES = c++ cc clang clang++ g++ gcc
INFERCLANG_WRAPPERS_PATHS = $(foreach cc,$(INFERCLANG_WRAPPERS_BASENAMES),$(LIB_DIR)/wrappers/$(cc))

$(INFERCLANG_WRAPPERS_PATHS): Makefile
	$(REMOVE) $@ && \
	cd $(@D) && \
	$(LN_S) ../../bin/InferClang $(@F)

$(BIN_DIR):
	$(MKDIR_P) $@

$(INFERTRACEBUGS_BIN_RELPATH): Makefile $(BIN_DIR)
	$(REMOVE) $@ && \
	cd $(@D) && \
	$(LN_S) ../lib/python/$(@F) $(@F)

.PHONY: src_build
src_build:
ifeq ($(IS_FACEBOOK_TREE),yes)
	$(MAKE) -C facebook
endif
	$(MAKE) -C $(SRC_DIR) infer
ifeq ($(BUILD_C_ANALYZERS),yes)
src_build: clang_plugin
endif

.PHONY: infer
infer: src_build
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
	$(MAKE) -C $(ANNOTATIONS_DIR)
endif
	$(MAKE) -C $(MODELS_DIR) all
ifeq ($(BUILD_C_ANALYZERS),yes)
infer: $(INFERCLANG_WRAPPERS_PATHS)
endif

.PHONY: clang_setup
clang_setup:
	export CC="$(CC)" CFLAGS="$(CFLAGS)"; \
	export CXX="$(CXX)" CXXFLAGS="$(CXXFLAGS)"; \
	export CPP="$(CPP)" LDFLAGS="$(LDFLAGS)" LIBS="$(LIBS)"; \
	$(FCP_DIR)/clang/setup.sh $(INFER_FCP_SETUP_OPTS)

.PHONY: clang_plugin
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

.PHONY: inferTraceBugs
inferTraceBugs: $(INFERTRACEBUGS_BIN_RELPATH)

.PHONY: test_this_build
test_this_build: clang_plugin
ifeq ($(IS_FACEBOOK_TREE),yes)
	$(MAKE) -C facebook
endif
	$(MAKE) -C $(SRC_DIR) test_build

.PHONY: test_oss_build
test_oss_build: clang_plugin
	$(MAKE) -C $(SRC_DIR) EXTRA_DEPS=opensource TEST_BUILD_DIR=$(BUILD_DIR)/opensource test_build

.PHONY: test_build
test_build: test_this_build
ifeq ($(IS_FACEBOOK_TREE),yes)
test_build: test_oss_build
endif

.PHONY: ocaml_unit_test
ocaml_unit_test: test_this_build
	$(TEST_BUILD_DIR)/unit/inferunit.byte

DIRECT_TESTS_REPLACE = $(patsubst %_frontend_test,%_frontend_replace,$(filter %_frontend_test,$(DIRECT_TESTS)))

.PHONY: frontend_replace
frontend_replace: $(DIRECT_TESTS_REPLACE)

define gen_direct_test_rule
.PHONY: $(1)
$(1): infer
	($(MAKE) -C \
	  $(INFER_DIR)/tests/codetoanalyze/$(shell printf $(1) | cut -f 1 -d _)/$(shell printf $(1) | cut -f 2 -d _) \
	  $(shell printf $(1) | cut -f 3 -d _) \
	3>&1 1>&2- 2>&3- ) \
	| grep -v "warning: ignoring old commands for target" \
	| grep -v "warning: overriding commands for target" \
	; exit $$$${PIPESTATUS[0]}

.PHONY: $(1)_clean
$(1)_clean:
	$(MAKE) -C \
	  $(INFER_DIR)/tests/codetoanalyze/$(shell printf $(1) | cut -f 1 -d _)/$(shell printf $(1) | cut -f 2 -d _) \
	  clean
endef

$(foreach test,$(DIRECT_TESTS) $(DIRECT_TESTS_REPLACE),\
    $(eval \
        $(call gen_direct_test_rule,$(test))))

.PHONY: direct_tests
direct_tests: $(DIRECT_TESTS)

define gen_build_system_test_rule
.PHONY: build_$(1)_test
build_$(1)_test: infer
	$(MAKE) -C $(INFER_DIR)/tests/build_systems/$(1) test

.PHONY: build_$(1)_clean
build_$(1)_clean:
	$(MAKE) -C $(INFER_DIR)/tests/build_systems/$(1) clean
endef

$(foreach test,$(BUILD_SYSTEMS_TESTS), $(eval $(call gen_build_system_test_rule,$(test))))

.PHONY: build_systems_tests
build_systems_tests: infer $(foreach test,$(BUILD_SYSTEMS_TESTS),build_$(test)_test)
	NO_BUCKD=1 $(INFER_DIR)/tests/build_systems/build_integration_tests.py

.PHONY: buck_test
buck_test: direct_tests build_systems_tests

.PHONY: inferTraceBugs_test
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

.PHONY: check_missing_mli
check_missing_mli:
	@for x in $$(find $(INFER_DIR)/src -name "*.ml" -or -name "*.re"); do \
	    test -f "$$x"i || echo Missing "$$x"i; done

.PHONY: toplevel
toplevel: infer
	$(MAKE) -C $(SRC_DIR) toplevel

.PHONY: inferScriptMode_test
inferScriptMode_test: toplevel
	$(call silent_on_success,\
	 INFER_REPL_BINARY=ocaml $(SCRIPT_DIR)/infer_repl $(INFER_DIR)/tests/repl/infer_batch_script.ml)

.PHONY: checkCopyright
checkCopyright:
	$(MAKE) -C $(SRC_DIR) checkCopyright

.PHONY: run-test
run-test: test_build ocaml_unit_test buck_test inferTraceBugs_test inferScriptMode_test checkCopyright
	$(MAKE) -C $(SRC_DIR) mod_dep.dot

.PHONY: test
test:
ifeq (,$(findstring s,$(MAKEFLAGS)))
	@$(MAKE) run-test && echo "ALL TESTS PASSED"
else
	@$(MAKE) run-test
endif

.PHONY: quick-test
quick-test: test_this_build ocaml_unit_test

.PHONY: test-replace
test-replace:
	@for file in $$(find $(INFER_DIR)/tests -name "*.exp.test"); do \
	    mv -f $$file $$(dirname $$file)/$$(basename -s .exp.test $$file).exp; done
	@for file in $$(find $(INFER_DIR)/tests -name "*.test.dot"); do \
	    mv -f $$file $$(dirname $$file)/$$(basename -s .test.dot $$file).dot; done
	INFER_RECORD_INTEGRATION_TESTS=1 NO_BUCKD=1 \
	  $(INFER_DIR)/tests/build_systems/build_integration_tests.py

.PHONY: uninstall
uninstall:
	$(REMOVE_DIR) $(DESTDIR)$(libdir)/infer/
	$(REMOVE) $(DESTDIR)$(bindir)/inferTraceBugs
	$(REMOVE) $(DESTDIR)$(bindir)/infer

.PHONY: test_clean
test_clean: $(foreach test,$(DIRECT_TESTS),$(test)_clean) \
            $(foreach test,$(BUILD_SYSTEMS_TESTS),build_$(test)_clean)

.PHONY: install
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
	(cd $(DESTDIR)$(libdir)/infer/infer/lib/wrappers/ && \
	 $(REMOVE) $(INFERCLANG_WRAPPERS_BASENAMES) && \
	 $(foreach cc,$(INFERCLANG_WRAPPERS_BASENAMES), \
	  $(LN_S) ../../bin/InferClang $(cc);))
	@for i in $$(find infer/lib/specs/*); do \
	  $(INSTALL_DATA) -C $$i $(DESTDIR)$(libdir)/infer/$$i; \
	done
	@for i in $$(find infer/models/cpp/include/ -not -type d); do \
		$(INSTALL_DATA) -C $$i $(DESTDIR)$(libdir)/infer/$$i; \
	done
	$(INSTALL_PROGRAM) -C $(INFERCLANG_BIN) $(DESTDIR)$(libdir)/infer/infer/bin/
	(cd $(DESTDIR)$(libdir)/infer/infer/bin/ && \
	 $(LN_S) -f InferClang InferClang++)
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
	$(INSTALL_PROGRAM) -C      $(LIB_DIR)/wrappers/javac \
	  $(DESTDIR)$(libdir)/infer/infer/lib/wrappers/
endif
	@for i in $$(find infer/lib/python/inferlib/* -type f); do \
	  $(INSTALL_DATA) -C $$i $(DESTDIR)$(libdir)/infer/$$i; \
	done
	$(INSTALL_PROGRAM) -C       infer/lib/python/infer.py \
	  $(DESTDIR)$(libdir)/infer/infer/lib/python/infer.py
	$(INSTALL_PROGRAM) -C       infer/lib/python/inferTraceBugs \
	  $(DESTDIR)$(libdir)/infer/infer/lib/python/inferTraceBugs
	$(INSTALL_PROGRAM) -C       infer/lib/python/report.py \
	  $(DESTDIR)$(libdir)/infer/infer/lib/python/report.py
	$(INSTALL_PROGRAM) -C $(INFER_BIN) $(DESTDIR)$(libdir)/infer/infer/bin/
	$(INSTALL_PROGRAM) -C $(INFERANALYZE_BIN) $(DESTDIR)$(libdir)/infer/infer/bin/
	$(INSTALL_PROGRAM) -C $(INFERPRINT_BIN) $(DESTDIR)$(libdir)/infer/infer/bin/
	(cd $(DESTDIR)$(bindir)/ && \
	 $(REMOVE) infer && \
	 $(LN_S) $(libdir)/infer/infer/bin/infer infer)
	(cd $(DESTDIR)$(bindir)/ && \
	 $(REMOVE) inferTraceBugs && \
	 $(LN_S) $(libdir)/infer/infer/lib/python/inferTraceBugs inferTraceBugs)

ifeq ($(IS_FACEBOOK_TREE),yes)
	$(MAKE) -C facebook install
endif

.PHONY: clean
clean: test_clean
ifeq ($(IS_RELEASE_TREE),no)
ifeq ($(BUILD_C_ANALYZERS),yes)
	$(MAKE) -C $(FCP_DIR) clean
	$(MAKE) -C $(FCP_DIR)/clang-ocaml clean
	$(REMOVE) $(INFERCLANG_WRAPPERS_PATHS)
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
	find $(INFER_DIR)/tests -name '*.o' -or -name '*.o.sh' -delete

.PHONY: conf-clean
conf-clean: clean
	$(REMOVE) $(PYTHON_DIR)/inferlib/*.pyc
	$(REMOVE) $(PYTHON_DIR)/inferlib/*/*.pyc
	$(REMOVE) .buckversion
	$(REMOVE) Makefile.config
	$(REMOVE) acinclude.m4
	$(REMOVE) aclocal.m4
	$(REMOVE_DIR) autom4te.cache/
	$(REMOVE) config.log
	$(REMOVE) config.status
	$(REMOVE) configure
	$(REMOVE_DIR) $(MODELS_DIR)/c/out/
	$(REMOVE_DIR) $(MODELS_DIR)/cpp/out/
	$(REMOVE_DIR) $(MODELS_DIR)/java/infer-out/
	$(REMOVE_DIR) $(MODELS_DIR)/objc/out/

# print any variable for Makefile debugging
print-%:
	@echo '$*=$($*)'

# print list of targets
.PHONY: show-targets
show-targets:
	@$(MAKE) -pqrR . | grep --only-matching -e '^[a-zA-Z0-9][^ ]*:' | cut -d ':' -f 1 | sort
