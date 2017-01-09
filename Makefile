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

BUILD_SYSTEMS_TESTS = \
  assembly ck_analytics ck_imports clang_multiple_files clang_translation clang_unknown_ext \
  delete_results_dir fail_on_issue gradle j1 javac linters make project_root_rel reactive \
  utf8_in_procname utf8_in_pwd waf run_hidden_linters
ifneq ($(ANT),no)
BUILD_SYSTEMS_TESTS += ant
endif
ifneq ($(BUCK),no)
BUILD_SYSTEMS_TESTS += buck genrule
endif
ifneq ($(CMAKE),no)
BUILD_SYSTEMS_TESTS += clang_compilation_db cmake
endif
ifneq ($(NDKBUILD),no)
BUILD_SYSTEMS_TESTS += ndk_build
endif
ifneq ($(PYTHON_lxml),no)
BUILD_SYSTEMS_TESTS += results_xml
endif

DIRECT_TESTS=
ifeq ($(BUILD_C_ANALYZERS),yes)
DIRECT_TESTS += c_errors c_frontend cpp_checkers cpp_errors cpp_frontend cpp_quandary
endif
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
DIRECT_TESTS += \
  java_checkers java_eradicate java_infer java_tracing java_quandary java_threadsafety \
  java_crashcontext java_harness
endif
ifneq ($(XCODE_SELECT),no)
DIRECT_TESTS += objc_frontend objc_errors objc_linters objc_ioslinters objcpp_frontend objcpp_linters
endif

.PHONY: all
all: infer

.PHONY: src_build
src_build:
ifeq ($(IS_FACEBOOK_TREE),yes)
	@$(MAKE) -C facebook setup
endif
	@$(MAKE) -C $(SRC_DIR) infer
ifeq ($(BUILD_C_ANALYZERS),yes)
src_build: clang_plugin
endif

.PHONY: infer
infer: src_build
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
	@$(MAKE) -C $(ANNOTATIONS_DIR)
endif
	@$(MAKE) -C $(MODELS_DIR) all

.PHONY: clang_setup
clang_setup:
	@export CC="$(CC)" CFLAGS="$(CFLAGS)"; \
	export CXX="$(CXX)" CXXFLAGS="$(CXXFLAGS)"; \
	export CPP="$(CPP)" LDFLAGS="$(LDFLAGS)" LIBS="$(LIBS)"; \
	$(FCP_DIR)/clang/setup.sh $(INFER_FCP_SETUP_OPTS) > /dev/null

.PHONY: clang_plugin
clang_plugin: clang_setup
ifeq ($(IS_RELEASE_TREE),no)
	@$(MAKE) -C $(FCP_DIR)/libtooling all \
	  CC=$(CC) CXX=$(CXX) \
	  CFLAGS="$(CFLAGS)" CXXFLAGS="$(CXXFLAGS)" \
	  CPP="$(CPP)" LDFLAGS="$(LDFLAGS)" LIBS="$(LIBS)" \
	  LOCAL_CLANG=$(CLANG_PREFIX)/bin/clang \
	  CLANG_PREFIX=$(CLANG_PREFIX) \
	  CLANG_INCLUDES=$(CLANG_INCLUDES)
	@$(MAKE) -C $(FCP_DIR)/clang-ocaml all \
          build/clang_ast_proj.ml build/clang_ast_proj.mli \
	  CC=$(CC) CXX=$(CXX) \
	  CFLAGS="$(CFLAGS)" CXXFLAGS="$(CXXFLAGS)" \
	  CPP="$(CPP)" LDFLAGS="$(LDFLAGS)" LIBS="$(LIBS)" \
	  LOCAL_CLANG=$(CLANG_PREFIX)/bin/clang \
	  CLANG_PREFIX=$(CLANG_PREFIX) \
	  CLANG_INCLUDES=$(CLANG_INCLUDES)
endif

.PHONY: test_build
test_build: clang_plugin
ifeq ($(IS_FACEBOOK_TREE),yes)
	@$(MAKE) -C facebook setup
endif
	@$(MAKE) -C $(SRC_DIR) test_build

.PHONY: ocaml_unit_test
ocaml_unit_test: test_build
	$(call silent_on_success,$(TEST_BUILD_DIR)/unit/inferunit.byte)

DIRECT_TESTS_REPLACE = $(patsubst %_frontend,%_frontend_replace,$(filter %_frontend,$(DIRECT_TESTS)))

define silence_make
  ($(1) 2> >(grep -v "warning: \(ignoring old\|overriding\) \(commands\|recipe\) for target") \
  ; exit $${PIPESTATUS[0]})
endef

.PHONY: frontend_replace
frontend_replace: $(DIRECT_TESTS_REPLACE)

.PHONY: $(DIRECT_TESTS:%=direct_%_test)
$(DIRECT_TESTS:%=direct_%_test): infer
	@$(call silence_make,\
	$(MAKE) -C \
	  $(INFER_DIR)/tests/codetoanalyze/$(shell printf $@ | cut -f 2 -d _)/$(shell printf $@ | cut -f 3 -d _) \
	  test)

.PHONY: $(DIRECT_TESTS:%=direct_%_print)
$(DIRECT_TESTS:%=direct_%_print): infer
	@$(call silence_make,\
	$(MAKE) -C \
	  $(INFER_DIR)/tests/codetoanalyze/$(shell printf $@ | cut -f 2 -d _)/$(shell printf $@ | cut -f 3 -d _) \
	  print)

.PHONY: $(DIRECT_TESTS:%=direct_%_clean)
$(DIRECT_TESTS:%=direct_%_clean):
	@$(call silence_make,\
	$(MAKE) -C \
	  $(INFER_DIR)/tests/codetoanalyze/$(shell printf $@ | cut -f 2 -d _)/$(shell printf $@ | cut -f 3 -d _) \
	  clean)

.PHONY: direct_tests
direct_tests: $(DIRECT_TESTS:%=direct_%_test)

# do not run these two tests in parallel otherwise Buck has a bad time
build_genrule_test: build_buck_test
build_genrule_print: build_buck_print

# the waf test and the make test run the same `make` command
build_waf_test: build_make_test
build_waf_print: build_make_print

.PHONY: print_direct_tests
print_direct_tests: $(DIRECT_TESTS:%=direct_%_print)

.PHONY: $(BUILD_SYSTEMS_TESTS:%=build_%_test)
$(BUILD_SYSTEMS_TESTS:%=build_%_test): infer
	@$(call silence_make,\
	$(MAKE) -C $(INFER_DIR)/tests/build_systems/$(patsubst build_%_test,%,$@) test)

.PHONY: $(BUILD_SYSTEMS_TESTS:%=build_%_print)
$(BUILD_SYSTEMS_TESTS:%=build_%_print): infer
	@$(call silence_make,\
	$(MAKE) -C $(INFER_DIR)/tests/build_systems/$(patsubst build_%_print,%,$@) print)

.PHONY: $(BUILD_SYSTEMS_TESTS:%=build_%_clean)
$(BUILD_SYSTEMS_TESTS:%=build_%_clean):
	@$(call silence_make,\
	$(MAKE) -C $(INFER_DIR)/tests/build_systems/$(patsubst build_%_clean,%,$@) clean)

.PHONY: build_systems_tests
build_systems_tests: $(BUILD_SYSTEMS_TESTS:%=build_%_test)

.PHONY: print_build_systems_tests
print_build_systems_tests: $(BUILD_SYSTEMS_TESTS:%=build_%_print)

.PHONY: endtoend_test
endtoend_test: print_direct_tests print_build_systems_tests
# pre-compute all the results first so that the test failures show up near the end of the output
	$(MAKE) direct_tests build_systems_tests

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
toplevel: clang_plugin
	@$(MAKE) -C $(SRC_DIR) toplevel

.PHONY: inferScriptMode_test
inferScriptMode_test: test_build
	$(call silent_on_success,\
	 INFER_REPL_BINARY=ocaml $(SCRIPT_DIR)/infer_repl $(INFER_DIR)/tests/repl/infer_batch_script.ml)

.PHONY: checkCopyright
checkCopyright:
	@$(MAKE) -C $(SRC_DIR) checkCopyright

.PHONY: validate-skel
validate-skel:
ifeq ($(IS_FACEBOOK_TREE),yes)
	@$(MAKE) -C facebook validate
endif


.PHONY: test
test: test_build ocaml_unit_test endtoend_test inferTraceBugs_test inferScriptMode_test \
      checkCopyright validate-skel
	@$(MAKE) -C $(SRC_DIR) mod_dep.dot
ifeq (,$(findstring s,$(MAKEFLAGS)))
	@echo "ALL TESTS PASSED"
endif

.PHONY: quick-test
quick-test: test_build ocaml_unit_test

.PHONY: test-replace
test-replace:
	@$(MAKE) -k endtoend_test || true
	@for file in $$(find $(INFER_DIR)/tests -name "*.exp.test"); do \
	    mv -f $$file $$(dirname $$file)/$$(basename -s .exp.test $$file).exp; done
	@for file in $$(find $(INFER_DIR)/tests -name "*.test.dot"); do \
	    mv -f $$file $$(dirname $$file)/$$(basename -s .test.dot $$file).dot; done

.PHONY: uninstall
uninstall:
	$(REMOVE_DIR) $(DESTDIR)$(libdir)/infer/
	$(REMOVE) $(DESTDIR)$(bindir)/infer

.PHONY: test_clean
test_clean: $(DIRECT_TESTS:%=direct_%_clean) $(BUILD_SYSTEMS_TESTS:%=build_%_clean)

.PHONY: install
install: infer
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
	test -d      $(DESTDIR)$(libdir)/infer/infer/lib/linter_rules/ || \
	  $(MKDIR_P) $(DESTDIR)$(libdir)/infer/infer/lib/linter_rules
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
#	  only for files that point to InferClang
	(cd $(DESTDIR)$(libdir)/infer/infer/lib/wrappers/ && \
	 $(foreach cc,$(shell find $(LIB_DIR)/wrappers -type l), \
	  [ $(cc) -ef $(INFERCLANG_BIN) ] && \
	  $(REMOVE) $(notdir $(cc)) && \
	  $(LN_S) ../../bin/InferClang $(notdir $(cc));))
	@for i in $$(find infer/lib/specs/*); do \
	  $(INSTALL_DATA) -C $$i $(DESTDIR)$(libdir)/infer/$$i; \
	done
	@for i in $$(find infer/models/cpp/include/ -not -type d); do \
		$(INSTALL_DATA) -C $$i $(DESTDIR)$(libdir)/infer/$$i; \
	done
	$(INSTALL_DATA) -C          infer/lib/linter_rules/linters.al \
	  $(DESTDIR)$(libdir)/infer/infer/lib/linter_rules/linters.al
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
	@$(MAKE) -C facebook install
endif

.PHONY: clean
clean: test_clean
ifeq ($(IS_RELEASE_TREE),no)
ifeq ($(BUILD_C_ANALYZERS),yes)
	@$(MAKE) -C $(FCP_DIR) clean
	@$(MAKE) -C $(FCP_DIR)/clang-ocaml clean
endif
endif
	@$(MAKE) -C $(SRC_DIR) clean
	@$(MAKE) -C $(ANNOTATIONS_DIR) clean
	@$(MAKE) -C $(MODELS_DIR) clean
ifeq ($(IS_FACEBOOK_TREE),yes)
	@$(MAKE) -C facebook clean
endif
	@$(MAKE) -C $(DEPENDENCIES_DIR)/ocamldot clean
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
