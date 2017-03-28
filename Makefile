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

ifeq ($(BUILD_C_ANALYZERS),yes)
BUILD_SYSTEMS_TESTS += \
  assembly \
  ck_analytics ck_imports \
  clang_compilation_db_escaped clang_compilation_db_relpath \
  clang_multiple_files \
  clang_translation \
  clang_unknown_ext \
  clang_with_E_flag \
  clang_with_M_flag \
  clang_with_MD_flag \
  delete_results_dir \
  differential_resolve_infer_eradicate_conflict \
  differential_skip_anonymous_class_renamings \
  differential_skip_duplicated_types_on_filenames \
  differential_skip_duplicated_types_on_filenames_with_renamings \
  fail_on_issue \
  j1 \
  linters \
  make \
  project_root_rel \
  reactive \
  run_hidden_linters \
  utf8_in_procname \
  waf \

ifneq ($(BUCK),no)
BUILD_SYSTEMS_TESTS += buck-clang-db
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
ifneq ($(XCODE_SELECT),no)
BUILD_SYSTEMS_TESTS += xcodebuild_no_xcpretty
ifneq ($(XCPRETTY),no)
BUILD_SYSTEMS_TESTS += xcodebuild
endif
endif # XCODE_SELECT
endif # BUILD_C_ANALYZERS

ifeq ($(BUILD_JAVA_ANALYZERS),yes)
BUILD_SYSTEMS_TESTS += gradle javac utf8_in_pwd
ifneq ($(ANT),no)
BUILD_SYSTEMS_TESTS += ant
endif
ifneq ($(BUCK),no)
BUILD_SYSTEMS_TESTS += buck genrule
endif
ifneq ($(MVN),no)
BUILD_SYSTEMS_TESTS += mvn
endif
endif

ifeq ($(BUILD_C_ANALYZERS),yes)
DIRECT_TESTS += \
  c_errors c_frontend c_bufferoverrun cpp_checkers cpp_errors cpp_frontend cpp_quandary
endif
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
DIRECT_TESTS += \
  java_checkers java_eradicate java_infer java_tracing java_quandary java_threadsafety \
  java_crashcontext java_harness
endif
ifneq ($(XCODE_SELECT),no)
DIRECT_TESTS += \
  objc_frontend objc_errors objc_linters objc_ioslints objcpp_frontend objcpp_linters objc_linters-for-test-only
endif

.PHONY: all
all: infer

configure: configure.ac $(wildcard m4/*.m4)
#	rerun ./autogen.sh in case of failure as the failure may be due to needing to rerun
#	./configure
	$(QUIET)($(call silent_on_success,Generate ./configure,./autogen.sh)) || \
	./autogen.sh

Makefile.autoconf: configure Makefile.autoconf.in
#	rerun ./configure with the flags that were used last time it was run (if available)
#	retry in case of failure as the failure may be due to needing to rerun ./configure
	$(QUIET)($(call silent_on_success,Running\
	./configure $(shell ./config.status --config || true),\
	./configure $(shell ./config.status --config || true))) || \
	./configure $(shell ./config.status --config || true)

.PHONY: fb-setup
fb-setup:
	$(QUIET)$(call silent_on_success,Facebook setup,\
	$(MAKE) -C facebook setup)

.PHONY: src_build
src_build:
	$(QUIET)$(call silent_on_success,Building native Infer,\
	$(MAKE) -C $(SRC_DIR) infer)

.PHONY: byte
byte:
	$(QUIET)$(call silent_on_success,Building byte Infer,\
	$(MAKE) -C $(SRC_DIR) byte)

.PHONY: test_build
test_build:
	$(QUIET)$(call silent_on_success,Testing Infer builds without warnings,\
	$(MAKE) -C $(SRC_DIR) TEST=1 byte_no_install)
#	byte_no_install builds most of what toplevel needs, so it's more efficient to run the
#	toplevel build straight after it rather than in parallel. Note that both targets build files
#	that the other doesn't.
	$(QUIET)$(call silent_on_success,Testing Infer toplevel builds,\
	$(MAKE) -C $(SRC_DIR) TEST=1 toplevel)

ifeq ($(IS_FACEBOOK_TREE),yes)
byte src_build test_build: fb-setup
endif

ifeq ($(BUILD_C_ANALYZERS),yes)
byte src_build test_build: clang_plugin
endif

.PHONY: infer
infer: src_build
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
	$(QUIET)$(call silent_on_success,Building Java annotations,\
	$(MAKE) -C $(ANNOTATIONS_DIR))
endif
	$(QUIET)$(call silent_on_success,Building Infer models,\
	$(MAKE) -C $(MODELS_DIR) all)

.PHONY: clang_setup
clang_setup:
	$(QUIET)export CC="$(CC)" CFLAGS="$(CFLAGS)"; \
	export CXX="$(CXX)" CXXFLAGS="$(CXXFLAGS)"; \
	export CPP="$(CPP)" LDFLAGS="$(LDFLAGS)" LIBS="$(LIBS)"; \
	$(FCP_DIR)/clang/setup.sh --only-check-install || \
	$(FCP_DIR)/clang/setup.sh $(INFER_FCP_SETUP_OPTS)

.PHONY: clang_plugin
clang_plugin: clang_setup
ifeq ($(IS_RELEASE_TREE),no)
	$(QUIET)$(call silent_on_success,Building clang plugin,\
	$(MAKE) -C $(FCP_DIR)/libtooling all \
	  CC=$(CC) CXX=$(CXX) \
	  CFLAGS="$(CFLAGS)" CXXFLAGS="$(CXXFLAGS)" \
	  CPP="$(CPP)" LDFLAGS="$(LDFLAGS)" LIBS="$(LIBS)" \
	  LOCAL_CLANG=$(CLANG_PREFIX)/bin/clang \
	  CLANG_PREFIX=$(CLANG_PREFIX) \
	  CLANG_INCLUDES=$(CLANG_INCLUDES))
	$(QUIET)$(call silent_on_success,Building clang plugin OCaml interface,\
	$(MAKE) -C $(FCP_DIR)/clang-ocaml all \
          build/clang_ast_proj.ml build/clang_ast_proj.mli \
	  CC=$(CC) CXX=$(CXX) \
	  CFLAGS="$(CFLAGS)" CXXFLAGS="$(CXXFLAGS)" \
	  CPP="$(CPP)" LDFLAGS="$(LDFLAGS)" LIBS="$(LIBS)" \
	  LOCAL_CLANG=$(CLANG_PREFIX)/bin/clang \
	  CLANG_PREFIX=$(CLANG_PREFIX) \
	  CLANG_INCLUDES=$(CLANG_INCLUDES))
endif

.PHONY: ocaml_unit_test
ocaml_unit_test: test_build
	$(QUIET)$(call silent_on_success,Running OCaml unit tests,\
	$(BUILD_DIR)/test/infer/unit/inferunit.byte)

define silence_make
  ($(1) 2> >(grep -v "warning: \(ignoring old\|overriding\) \(commands\|recipe\) for target") \
  ; exit $${PIPESTATUS[0]})
endef

.PHONY: $(DIRECT_TESTS:%=direct_%_test)
$(DIRECT_TESTS:%=direct_%_test): infer
	$(QUIET)$(call silent_on_success,Running $(subst _, ,$@),\
	$(call silence_make,\
	$(MAKE) -C \
	  $(INFER_DIR)/tests/codetoanalyze/$(shell printf $@ | cut -f 2 -d _)/$(shell printf $@ | cut -f 3 -d _) \
	  test))

.PHONY: $(DIRECT_TESTS:%=direct_%_print)
$(DIRECT_TESTS:%=direct_%_print): infer
	$(QUIET)$(call silence_make,\
	$(MAKE) -C \
	  $(INFER_DIR)/tests/codetoanalyze/$(shell printf $@ | cut -f 2 -d _)/$(shell printf $@ | cut -f 3 -d _) \
	  print)

.PHONY: $(DIRECT_TESTS:%=direct_%_clean)
$(DIRECT_TESTS:%=direct_%_clean):
	$(QUIET)$(call silence_make,\
	$(MAKE) -C \
	  $(INFER_DIR)/tests/codetoanalyze/$(shell printf $@ | cut -f 2 -d _)/$(shell printf $@ | cut -f 3 -d _) \
	  clean)

.PHONY: $(DIRECT_TESTS:%=direct_%_replace)
$(DIRECT_TESTS:%=direct_%_replace): infer
	$(QUIET)$(call silence_make,\
	$(MAKE) -C \
	  $(INFER_DIR)/tests/codetoanalyze/$(shell printf $@ | cut -f 2 -d _)/$(shell printf $@ | cut -f 3 -d _) \
	  replace)

.PHONY: direct_tests
direct_tests: $(DIRECT_TESTS:%=direct_%_test)

# do not run these two tests in parallel otherwise Buck has a bad time
build_genrule_test: build_buck_test
build_genrule_print: build_buck_print

# the waf test and the make test run the same `make` command
build_waf_test: build_make_test
build_waf_print: build_make_print

.PHONY: $(BUILD_SYSTEMS_TESTS:%=build_%_test)
$(BUILD_SYSTEMS_TESTS:%=build_%_test): infer
	$(QUIET)$(call silent_on_success,Running $(subst _, ,$@),\
	$(call silence_make,\
	$(MAKE) -C $(INFER_DIR)/tests/build_systems/$(patsubst build_%_test,%,$@) test))

.PHONY: $(BUILD_SYSTEMS_TESTS:%=build_%_print)
$(BUILD_SYSTEMS_TESTS:%=build_%_print): infer
	$(QUIET)$(call silence_make,\
	$(MAKE) -C $(INFER_DIR)/tests/build_systems/$(patsubst build_%_print,%,$@) print)

.PHONY: $(BUILD_SYSTEMS_TESTS:%=build_%_clean)
$(BUILD_SYSTEMS_TESTS:%=build_%_clean):
	$(QUIET)$(call silence_make,\
	$(MAKE) -C $(INFER_DIR)/tests/build_systems/$(patsubst build_%_clean,%,$@) clean)

.PHONY: $(BUILD_SYSTEMS_TESTS:%=build_%_replace)
$(BUILD_SYSTEMS_TESTS:%=build_%_replace): infer
	$(QUIET)$(call silence_make,\
	$(MAKE) -C $(INFER_DIR)/tests/build_systems/$(patsubst build_%_replace,%,$@) replace)

.PHONY: build_systems_tests
build_systems_tests: $(BUILD_SYSTEMS_TESTS:%=build_%_test)

.PHONY: endtoend_test
endtoend_test: $(BUILD_SYSTEMS_TESTS:%=build_%_test) $(DIRECT_TESTS:%=direct_%_test)

.PHONY: inferTraceBugs_test
inferTraceBugs_test: infer
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
	$(QUIET)$(call silent_on_success,Testing inferTraceBugs: running infer,\
	$(INFER_BIN) -o __test-infer-out__ -- \
	  $(JAVAC) $(EXAMPLES_DIR)/Hello.java)
else
	$(QUIET)$(call silent_on_success,Testing inferTraceBugs: running infer,\
	$(INFER_BIN) -o __test-infer-out__ -- \
	  clang -c $(EXAMPLES_DIR)/hello.c)
endif
	$(QUIET)$(REMOVE) Hello.class
	$(QUIET)$(call silent_on_success,Testing inferTraceBugs: --max-level=max,\
	$(PYTHON_DIR)/inferTraceBugs -o __test-infer-out__ \
	  --select 0 --max-level max)
	$(QUIET)$(call silent_on_success,Testing inferTraceBugs: --max-level=0,\
	$(PYTHON_DIR)/inferTraceBugs -o __test-infer-out__ \
	  --select 0 --max-level 0)
	$(QUIET)$(call silent_on_success,Testing inferTraceBugs: --max-level=max --no-source,\
	$(PYTHON_DIR)/inferTraceBugs -o __test-infer-out__ \
	  --select 0 --max-level max --no-source)
	$(QUIET)$(call silent_on_success,Testing inferTraceBugs: --only-show,\
	$(PYTHON_DIR)/inferTraceBugs -o __test-infer-out__ \
	  --only-show)
	$(QUIET)$(REMOVE_DIR) __test-infer-out__

.PHONY: check_missing_mli
check_missing_mli:
	$(QUIET)for x in $$(find $(INFER_DIR)/src -name "*.ml" -or -name "*.re"); do \
	    test -f "$$x"i || echo Missing "$$x"i; done

.PHONY: toplevel
toplevel: clang_plugin
	$(QUIET)$(MAKE) -C $(SRC_DIR) toplevel

.PHONY: inferScriptMode_test
inferScriptMode_test: test_build
	$(QUIET)$(call silent_on_success,Testing infer OCaml REPL,\
	 INFER_REPL_BINARY=ocaml $(SCRIPT_DIR)/infer_repl $(INFER_DIR)/tests/repl/infer_batch_script.ml)

.PHONY: checkCopyright
checkCopyright:
	$(QUIET)$(call silent_on_success,Building checkCopyright,\
	$(MAKE) -C $(SRC_DIR) checkCopyright)

.PHONY: validate-skel
validate-skel:
ifeq ($(IS_FACEBOOK_TREE),yes)
	$(QUIET)$(call silent_on_success,Validating facebook/,\
	$(MAKE) -C facebook validate)
endif


.PHONY: test
test: test_build ocaml_unit_test endtoend_test inferTraceBugs_test inferScriptMode_test \
      checkCopyright validate-skel
	$(QUIET)$(call silent_on_success,Building Infer source dependency graph,\
	$(MAKE) -C $(SRC_DIR) mod_dep.dot)
ifeq (,$(findstring s,$(MAKEFLAGS)))
	$(QUIET)echo "$(TERM_INFO)ALL TESTS PASSED$(TERM_RESET)"
endif

.PHONY: quick-test
quick-test: test_build ocaml_unit_test

.PHONY: test-replace
test-replace: $(BUILD_SYSTEMS_TESTS:%=build_%_replace) $(DIRECT_TESTS:%=direct_%_replace)

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
	$(QUIET)for i in $$(find facebook-clang-plugins/clang/install -type d); do \
	  test -d      $(DESTDIR)$(libdir)/infer/$$i || \
	    $(MKDIR_P) $(DESTDIR)$(libdir)/infer/$$i; \
	done
	test -d      $(DESTDIR)$(libdir)/infer/infer/lib/clang_wrappers/ || \
	  $(MKDIR_P) $(DESTDIR)$(libdir)/infer/infer/lib/clang_wrappers/
	$(QUIET)for i in $$(find infer/models/cpp/include/ -type d); do \
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
	$(QUIET)for i in $$(find facebook-clang-plugins/clang/install -not -type d); do \
	  $(INSTALL_PROGRAM) -C $$i $(DESTDIR)$(libdir)/infer/$$i; \
	done
	$(QUIET)for i in $$(find infer/lib/clang_wrappers/*); do \
	  $(INSTALL_PROGRAM) -C $$i $(DESTDIR)$(libdir)/infer/$$i; \
	done
#	  only for files that point to InferClang
	(cd $(DESTDIR)$(libdir)/infer/infer/lib/wrappers/ && \
	 $(foreach cc,$(shell find $(LIB_DIR)/wrappers -type l), \
	  [ $(cc) -ef $(INFERCLANG_BIN) ] && \
	  $(REMOVE) $(notdir $(cc)) && \
	  $(LN_S) ../../bin/InferClang $(notdir $(cc));))
	$(QUIET)for i in $$(find infer/lib/specs/*); do \
	  $(INSTALL_DATA) -C $$i $(DESTDIR)$(libdir)/infer/$$i; \
	done
	$(QUIET)for i in $$(find infer/models/cpp/include/ -not -type d); do \
		$(INSTALL_DATA) -C $$i $(DESTDIR)$(libdir)/infer/$$i; \
	done
	$(INSTALL_DATA) -C          infer/lib/linter_rules/linters.al \
	  $(DESTDIR)$(libdir)/infer/infer/lib/linter_rules/linters.al
	$(INSTALL_PROGRAM) -C $(INFERCLANG_BIN) $(DESTDIR)$(libdir)/infer/infer/bin/
	(cd $(DESTDIR)$(libdir)/infer/infer/bin/ && \
	 $(LN_S) -f InferClang InferClang++)
endif
ifneq ($(XCODE_SELECT),no)
	$(QUIET)for i in $$(find infer/lib/xcode_wrappers/*); do \
	  $(INSTALL_PROGRAM) -C $$i $(DESTDIR)$(libdir)/infer/$$i; \
	done
endif
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
	$(INSTALL_DATA) -C          infer/annotations/annotations.jar \
	  $(DESTDIR)$(libdir)/infer/infer/annotations/annotations.jar
	$(QUIET)for i in infer/lib/java/*.jar; do \
	  $(INSTALL_DATA) -C $$i $(DESTDIR)$(libdir)/infer/$$i; \
	done
	$(INSTALL_PROGRAM) -C      $(LIB_DIR)/wrappers/javac \
	  $(DESTDIR)$(libdir)/infer/infer/lib/wrappers/
endif
	$(QUIET)for i in $$(find infer/lib/python/inferlib/* -type f); do \
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
	 $(LN_S) $(libdir_relative_to_bindir)/infer/infer/bin/infer infer)
	(cd $(DESTDIR)$(bindir)/ && \
	 $(REMOVE) inferTraceBugs && \
	 $(LN_S) $(libdir_relative_to_bindir)/infer/infer/lib/python/inferTraceBugs inferTraceBugs)

ifeq ($(IS_FACEBOOK_TREE),yes)
	$(QUIET)$(MAKE) -C facebook install
endif

.PHONY: clean
clean: test_clean
ifeq ($(IS_RELEASE_TREE),no)
ifeq ($(BUILD_C_ANALYZERS),yes)
	$(QUIET)$(MAKE) -C $(FCP_DIR) clean
	$(QUIET)$(MAKE) -C $(FCP_DIR)/clang-ocaml clean
endif
endif
	$(QUIET)$(MAKE) -C $(SRC_DIR) clean
	$(QUIET)$(MAKE) -C $(ANNOTATIONS_DIR) clean
	$(QUIET)$(MAKE) -C $(MODELS_DIR) clean
ifeq ($(IS_FACEBOOK_TREE),yes)
	$(QUIET)$(MAKE) -C facebook clean
endif
	$(QUIET)$(MAKE) -C $(DEPENDENCIES_DIR)/ocamldot clean
	find $(INFER_DIR)/tests \( -name '*.o' -o -name '*.o.sh' \) -delete
	$(REMOVE_DIR) _build_logs

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
	$(QUIET)echo '$*=$($*)'

# print list of targets
.PHONY: show-targets
show-targets:
	$(QUIET)$(MAKE) -pqrR . | grep --only-matching -e '^[a-zA-Z0-9][^ ]*:' | cut -d ':' -f 1 | sort
