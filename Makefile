# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

.PHONY: default
default: infer

ROOT_DIR = .
include $(ROOT_DIR)/Makefile.config

ORIG_SHELL_BUILD_MODE = $(BUILD_MODE)
# override this for faster builds (but slower infer)
BUILD_MODE ?= opt

MAKE_SOURCE = $(MAKE) -C $(SRC_DIR)

ifneq ($(UTOP),no)
BUILD_SYSTEMS_TESTS += infertop
endif

ifeq ($(BUILD_C_ANALYZERS),yes)
BUILD_SYSTEMS_TESTS += \
  annotation-reachability-sources-override \
  assembly \
  backtrack_level \
  ck_imports \
  clang_compilation_db_escaped clang_compilation_db_relpath \
  clang_multiple_files \
  clang_translation \
  clang_unknown_ext \
  clang_with_blacklisted_flags \
  clang_with_E_flag \
  clang_with_M_flag \
  clang_with_MD_flag \
  deduplicate_template_warnings \
  delete_results_dir \
  duplicate_symbols \
  fail_on_issue \
  j1 \
  linters \
  project_root_rel \
  reactive \
  results_xml \
  tracebugs \
  utf8_in_procname \
  export_changed_functions \
  incremental_analysis_remove_file \
  incremental_analysis_change_procedure \
  incremental_analysis_add_procedure \

DIRECT_TESTS += \
  c_biabduction \
  c_bufferoverrun \
  c_frontend \
  c_performance \
  c_pulse \
  c_purity \
  c_uninit \
  cpp_annotation-reachability \
  cpp_biabduction \
  cpp_bufferoverrun \
  cpp_conflicts \
  cpp_frontend \
  cpp_impurity \
  cpp_linters \
  cpp_linters-for-test-only \
  cpp_liveness \
  cpp_performance \
  cpp_pulse \
  cpp_quandary \
  cpp_racerd \
  cpp_siof \
  cpp_starvation \
  cpp_uninit \

ifneq ($(BUCK),no)
BUILD_SYSTEMS_TESTS += \
  buck_blacklist \
  buck-clang-db \
  buck_clang_test_determinator \
  buck_flavors \
  buck_flavors_diff \
  buck_flavors_run \
  buck_flavors_deterministic \
  buck_export_changed_functions \

endif
ifneq ($(CMAKE),no)
BUILD_SYSTEMS_TESTS += clang_compilation_db cmake inferconfig inferconfig_not_strict
endif
ifneq ($(NDKBUILD),no)
BUILD_SYSTEMS_TESTS += ndk_build
endif
ifeq ($(HAS_OBJC),yes)
BUILD_SYSTEMS_TESTS += \
  clang_test_determinator \
  objc_getters_setters \
  objc_missing_fld \
  objc_retain_cycles \
  objc_retain_cycles_weak

DIRECT_TESTS += \
  objc_biabduction \
  objc_frontend \
  objc_linters \
  objc_linters-def-folder \
  objc_linters-for-test-only \
  objc_liveness \
  objc_performance \
  objc_pulse \
  objc_quandary \
  objc_self-in-block \
  objc_uninit \
  objcpp_biabduction \
  objcpp_frontend \
  objcpp_linters \
  objcpp_linters-for-test-only \
  objcpp_liveness \
  objcpp_pulse \
  objcpp_racerd \
  objcpp_retain-cycles \

ifneq ($(XCODE_SELECT),no)
BUILD_SYSTEMS_TESTS += xcodebuild_no_xcpretty
endif
ifneq ($(XCPRETTY),no)
BUILD_SYSTEMS_TESTS += xcodebuild
endif
endif # HAS_OBJC
endif # BUILD_C_ANALYZERS

ifeq ($(BUILD_JAVA_ANALYZERS),yes)
BUILD_SYSTEMS_TESTS += \
  differential_interesting_paths_filter \
  differential_of_costs_report \
  incremental_analysis_cost_change \
  differential_skip_anonymous_class_renamings \
  differential_skip_duplicated_types_on_filenames \
  differential_skip_duplicated_types_on_filenames_with_renamings \
  gradle \
  java_test_determinator \
  javac \
  resource_leak_exception_lines \
  racerd_dedup

#TODO T41549034: Jdk11 translates string append differently, causing
#test failures in NullPointerExceptions:stringVarEqualsFalseNPE

DIRECT_TESTS += \
  java_annotreach \
  java_biabduction \
  java_bufferoverrun \
  java_checkers \
  java_nullsafe \
  java_hoisting \
  java_hoistingExpensive \
  java_impurity \
  java_inefficientKeysetIterator \
  java_litho-required-props \
  java_performance \
  java_performance-exclusive \
  java_pulse \
  java_purity \
  java_quandary \
  java_racerd \
  java_starvation \
  java_starvation-dedup \
  java_starvation-whole-program \
  java_topl \

ifeq ($(IS_FACEBOOK_TREE),yes)
DIRECT_TESTS += \
  java_fb-gk-interaction \
  java_fb-performance
endif

ifneq ($(ANT),no)
BUILD_SYSTEMS_TESTS += ant
endif



ifneq ($(BUCK),no)
BUILD_SYSTEMS_TESTS += genrulecapture buck_java_flavor
endif
ifneq ($(MVN),no)
BUILD_SYSTEMS_TESTS += mvn
endif
endif

ifeq ($(BUILD_C_ANALYZERS)+$(BUILD_JAVA_ANALYZERS),yes+yes)
BUILD_SYSTEMS_TESTS += make utf8_in_pwd waf
endif

ifeq ($(IS_INFER_RELEASE),no)
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
endif

.PHONY: fb-setup
fb-setup:
	$(QUIET)$(call silent_on_success,Facebook setup,\
	$(MAKE) -C facebook setup)

.PHONY: fmt
fmt:
	parallel $(OCAMLFORMAT_EXE) $(OCAMLFORMAT_ARGS) -i ::: $$(git diff --name-only --diff-filter=ACMRU $$(git merge-base origin/master HEAD) | grep "\.mli\?$$")

DUNE_ML:=$(shell find * -name 'dune*.in' | grep -v workspace | grep -v infer-source | grep -v infer/src/deadcode/dune.in)

.PHONY: fmt_dune
fmt_dune:
	parallel $(OCAMLFORMAT_EXE) $(OCAMLFORMAT_ARGS) -i ::: $(DUNE_ML)

SRC_ML:=$(shell find * \( -name _build -or -name facebook-clang-plugins -or -path facebook/dependencies -or -path sledge/llvm -or -path sledge/.llvm_build \) -not -prune -or -type f -and -name '*'.ml -or -name '*'.mli 2>/dev/null)

.PHONY: fmt_all
fmt_all:
	parallel $(OCAMLFORMAT_EXE) $(OCAMLFORMAT_ARGS) -i ::: $(SRC_ML) $(DUNE_ML)

# pre-building these avoids race conditions when building, eg src_build and test_build in parallel
.PHONY: src_build_common
src_build_common:
	$(QUIET)$(call silent_on_success,Generating source dependencies,\
	$(MAKE_SOURCE) src_build_common)

.PHONY: src_build
src_build: src_build_common
	$(QUIET)$(call silent_on_success,Building native($(BUILD_MODE)) Infer,\
	$(MAKE_SOURCE) infer)

.PHONY: byte
byte: src_build_common
	$(QUIET)$(call silent_on_success,Building byte Infer,\
	$(MAKE_SOURCE) byte)

.PHONY: check
check: src_build_common
	$(QUIET)$(call silent_on_success,Building artifacts for tooling support,\
	$(MAKE_SOURCE) check)

.PHONY: test_build
test_build: src_build_common
	$(QUIET)$(call silent_on_success,Testing Infer builds without warnings,\
	$(MAKE_SOURCE) test)

# deadcode analysis: only do the deadcode detection on Facebook builds and if GNU sed is available
.PHONY: real_deadcode
real_deadcode: src_build_common
	$(QUIET)$(call silent_on_success,Testing there is no dead OCaml code,\
	$(MAKE) -C $(SRC_DIR)/deadcode)

.PHONY: deadcode
deadcode:
ifeq ($(IS_FACEBOOK_TREE),no)
	$(QUIET)echo "Deadcode detection only works in Facebook builds, skipping"
endif
ifeq ($(GNU_SED),no)
	$(QUIET)echo "Deadcode detection only works with GNU sed installed, skipping"
endif

ifeq ($(IS_FACEBOOK_TREE),yes)
ifneq ($(GNU_SED),no)
deadcode: real_deadcode
endif
endif


.PHONY: toplevel toplevel_test
toplevel toplevel_test: src_build_common

toplevel:
	$(QUIET)$(call silent_on_success,Building Infer REPL,\
	$(MAKE_SOURCE) toplevel)
	$(QUIET)echo
	$(QUIET)echo "You can now use the infer REPL:"
	$(QUIET)echo "  \"$(ABSOLUTE_ROOT_DIR)/scripts/infer_repl\""

toplevel_test:
	$(QUIET)$(call silent_on_success,Building Infer REPL,\
	$(MAKE_SOURCE) toplevel)

ifeq ($(IS_FACEBOOK_TREE),yes)
byte src_build_common src_build test_build: fb-setup
endif

ifeq ($(BUILD_C_ANALYZERS),yes)
byte src_build src_build_common test_build: clang_plugin
endif

ifneq ($(NINJA),no)
FCP_COMPILE_ARGS = --ninja --sequential-link
endif

$(INFER_COMMAND_MANUALS): src_build $(MAKEFILE_LIST)
	$(QUIET)$(MKDIR_P) $(@D)
	$(QUIET)$(INFER_BIN) $(patsubst infer-%.1,%,$(@F)) --help-scrubbed --help-format=groff > $@

$(INFER_COMMAND_TEXT_MANUALS): src_build $(MAKEFILE_LIST)
	$(QUIET)$(MKDIR_P) $(@D)
	$(QUIET)$(INFER_BIN) $(patsubst infer-%.txt,%,$(@F)) --help-scrubbed --help-format=plain > $@

$(INFER_MANUAL): src_build $(MAKEFILE_LIST)
	$(QUIET)$(MKDIR_P) $(@D)
	$(QUIET)$(INFER_BIN) --help-scrubbed --help-format=groff > $@

$(INFER_TEXT_MANUAL): src_build $(MAKEFILE_LIST)
	$(QUIET)$(MKDIR_P) $(@D)
	$(QUIET)$(INFER_BIN) --help-scrubbed --help-format=plain > $@

$(INFER_FULL_TEXT_MANUAL): src_build $(MAKEFILE_LIST)
	$(QUIET)$(MKDIR_P) $(@D)
	$(QUIET)$(INFER_BIN) --help-scrubbed-full --help-format=plain > $@

$(INFER_GROFF_MANUALS_GZIPPED): %.gz: %
	$(QUIET)$(REMOVE) $@
	gzip $<

manuals:
	$(QUIET)$(call silent_on_success,Building Infer manuals,\
	$(MAKE) $(INFER_MANUALS))

infer_models: src_build
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
	$(QUIET)$(call silent_on_success,Building infer annotations,\
	$(MAKE) -C $(ANNOTATIONS_DIR))
endif
	$(QUIET)$(call silent_on_success,Building infer models,\
	$(MAKE) -C $(MODELS_DIR) all)

.PHONY: infer byte_infer
infer byte_infer: infer_models
infer: src_build
byte_infer: byte

.PHONY: opt
opt:
	$(QUIET)$(MAKE) BUILD_MODE=opt infer

.PHONY: clang_setup
clang_setup:
#	if clang is already built then let the user know they might not need to rebuild clang
	$(QUIET)export CC="$(CC)" CFLAGS="$(CFLAGS)"; \
	export CXX="$(CXX)" CXXFLAGS="$(CXXFLAGS)"; \
	export CPP="$(CPP)" LDFLAGS="$(LDFLAGS)" LIBS="$(LIBS)"; \
	$(FCP_DIR)/clang/setup.sh --only-check-install || { \
	  if [ -x '$(FCP_DIR)'/clang/install/bin/clang ]; then \
	    echo '$(TERM_INFO)*** Now building clang, this will take a while...$(TERM_RESET)' >&2; \
	    echo '$(TERM_INFO)*** If you believe that facebook-clang-plugins/clang/install is up-to-date you can$(TERM_RESET)' >&2; \
	    echo '$(TERM_INFO)*** interrupt the compilation (Control-C) and run this to prevent clang from being rebuilt:$(TERM_RESET)' >&2; \
	    echo >&2 ; \
	    echo '$(TERM_INFO)      $(FCP_DIR)/clang/setup.sh --only-record-install$(TERM_RESET)' >&2; \
	    echo >&2 ; \
	    echo '$(TERM_INFO)(TIP: you can also force a clang rebuild by removing $(FCP_DIR)/clang/installed.version)$(TERM_RESET)' >&2; \
	    echo >&2 ; \
	  fi; \
	  $(FCP_DIR)/clang/setup.sh $(FCP_COMPILE_ARGS); \
	}

.PHONY: clang_plugin
clang_plugin: clang_setup
	$(QUIET)$(call silent_on_success,Building clang plugin,\
	$(MAKE) -C $(FCP_DIR)/libtooling all \
	  CC="$(CC)" CXX="$(CXX)" \
	  CFLAGS="$(CFLAGS)" CXXFLAGS="$(CXXFLAGS)" \
	  CPP="$(CPP)" LDFLAGS="$(LDFLAGS)" LIBS="$(LIBS)" \
	  LOCAL_CLANG=$(CLANG_PREFIX)/bin/clang \
	  CLANG_PREFIX=$(CLANG_PREFIX) \
	  CLANG_INCLUDES=$(CLANG_INCLUDES) \
	  SDKPATH=$(XCODE_ISYSROOT) \
	)
	$(QUIET)$(call silent_on_success,Building clang plugin OCaml interface,\
	$(MAKE) -C $(FCP_DIR)/clang-ocaml all \
          build/clang_ast_proj.ml build/clang_ast_proj.mli \
	  CC=$(CC) CXX=$(CXX) \
	  CFLAGS="$(CFLAGS)" CXXFLAGS="$(CXXFLAGS)" \
	  CPP="$(CPP)" LDFLAGS="$(LDFLAGS)" LIBS="$(LIBS)" \
	  LOCAL_CLANG=$(CLANG_PREFIX)/bin/clang \
	  CLANG_PREFIX=$(CLANG_PREFIX) \
	  CLANG_INCLUDES=$(CLANG_INCLUDES) \
	  SDKPATH=$(XCODE_ISYSROOT) \
	)
.PHONY: clang_plugin_test
clang_plugin_test: clang_setup
	$(QUIET)$(call silent_on_success,Running facebook-clang-plugins/libtooling/ tests,\
	$(MAKE) -C $(FCP_DIR)/libtooling test \
	  CC=$(CC) CXX=$(CXX) \
	  CFLAGS="$(CFLAGS)" CXXFLAGS="$(CXXFLAGS)" \
	  CPP="$(CPP)" LDFLAGS="$(LDFLAGS)" LIBS="$(LIBS)" \
	  LOCAL_CLANG=$(CLANG_PREFIX)/bin/clang \
	  CLANG_PREFIX=$(CLANG_PREFIX) \
	  CLANG_INCLUDES=$(CLANG_INCLUDES) \
	  SDKPATH=$(XCODE_ISYSROOT) \
	)
	$(QUIET)$(call silent_on_success,Running facebook-clang-plugins/clang-ocaml/ tests,\
	$(MAKE) -C $(FCP_DIR)/clang-ocaml test \
	  CC=$(CC) CXX=$(CXX) \
	  CFLAGS="$(CFLAGS)" CXXFLAGS="$(CXXFLAGS)" \
	  CPP="$(CPP)" LDFLAGS="$(LDFLAGS)" LIBS="$(LIBS)" \
	  LOCAL_CLANG=$(CLANG_PREFIX)/bin/clang \
	  CLANG_PREFIX=$(CLANG_PREFIX) \
	  CLANG_INCLUDES=$(CLANG_INCLUDES) \
	  SDKPATH=$(XCODE_ISYSROOT) \
	)

.PHONY: clang_plugin_test
clang_plugin_test_replace: clang_setup
	$(QUIET)$(call silent_on_success,Running facebook-clang-plugins/libtooling/ record tests,\
	$(MAKE) -C $(FCP_DIR)/libtooling record-test-outputs \
	  CC=$(CC) CXX=$(CXX) \
	  CFLAGS="$(CFLAGS)" CXXFLAGS="$(CXXFLAGS)" \
	  CPP="$(CPP)" LDFLAGS="$(LDFLAGS)" LIBS="$(LIBS)" \
	  LOCAL_CLANG=$(CLANG_PREFIX)/bin/clang \
	  CLANG_PREFIX=$(CLANG_PREFIX) \
	  CLANG_INCLUDES=$(CLANG_INCLUDES) \
	  SDKPATH=$(XCODE_ISYSROOT) \
	)
	$(QUIET)$(call silent_on_success,Running facebook-clang-plugins/clang-ocaml/ record tests,\
	$(MAKE) -C $(FCP_DIR)/clang-ocaml record-test-outputs \
	  CC=$(CC) CXX=$(CXX) \
	  CFLAGS="$(CFLAGS)" CXXFLAGS="$(CXXFLAGS)" \
	  CPP="$(CPP)" LDFLAGS="$(LDFLAGS)" LIBS="$(LIBS)" \
	  LOCAL_CLANG=$(CLANG_PREFIX)/bin/clang \
	  CLANG_PREFIX=$(CLANG_PREFIX) \
	  CLANG_INCLUDES=$(CLANG_INCLUDES) \
	  SDKPATH=$(XCODE_ISYSROOT) \
	)

.PHONY: ocaml_unit_test
ocaml_unit_test: test_build
	$(QUIET)$(REMOVE_DIR) infer-out-unit-tests
	$(QUIET)$(call silent_on_success,Running OCaml unit tests,\
	INFER_ARGS=--results-dir^infer-out-unit-tests $(INFERUNIT_BIN))

define silence_make
  $(1) 2> >(grep -v 'warning: \(ignoring old\|overriding\) \(commands\|recipe\) for target')
endef

.PHONY: $(DIRECT_TESTS:%=direct_%_test)
$(DIRECT_TESTS:%=direct_%_test): infer
	$(QUIET)$(call silent_on_success,Running test: $(subst _, ,$@),\
	$(call silence_make,\
	$(MAKE) -C \
	  $(INFER_DIR)/tests/codetoanalyze/$(shell printf $@ | cut -f 2 -d _)/$(shell printf $@ | cut -f 3 -d _) \
	  test))

.PHONY: $(DIRECT_TESTS:%=direct_%_print)
$(DIRECT_TESTS:%=direct_%_print): infer
	$(QUIET)$(call silent_on_success,Running: $(subst _, ,$@),\
	$(call silence_make,\
	$(MAKE) -C \
	  $(INFER_DIR)/tests/codetoanalyze/$(shell printf $@ | cut -f 2 -d _)/$(shell printf $@ | cut -f 3 -d _) \
	  print))

.PHONY: $(DIRECT_TESTS:%=direct_%_clean)
$(DIRECT_TESTS:%=direct_%_clean):
	$(QUIET)$(call silent_on_success,Cleaning: $(subst _, ,$@),\
	$(call silence_make,\
	$(MAKE) -C \
	  $(INFER_DIR)/tests/codetoanalyze/$(shell printf $@ | cut -f 2 -d _)/$(shell printf $@ | cut -f 3 -d _) \
	  clean))

.PHONY: $(DIRECT_TESTS:%=direct_%_replace)
$(DIRECT_TESTS:%=direct_%_replace): infer
	$(QUIET)$(call silent_on_success,Recording: $(subst _, ,$@),\
	$(call silence_make,\
	$(MAKE) -C \
	  $(INFER_DIR)/tests/codetoanalyze/$(shell printf $@ | cut -f 2 -d _)/$(shell printf $@ | cut -f 3 -d _) \
	  replace))

.PHONY: direct_tests
direct_tests: $(DIRECT_TESTS:%=direct_%_test)

COST_TESTS += \
  c_performance \
  java_hoistingExpensive \
  java_performance \
  java_performance-exclusive \
  objc_performance \

ifeq ($(IS_FACEBOOK_TREE),yes)
   COST_TESTS += java_fb-performance
endif

.PHONY: cost_tests
cost_tests: $(COST_TESTS:%=direct_%_test)

.PHONY: cost_tests_clean
cost_tests_clean: $(COST_TESTS:%=direct_%_clean)

.PHONY: cost_tests_replace
cost_tests_replace: $(COST_TESTS:%=direct_%_replace)

.PHONY: cost_tests_print
cost_tests_print: $(COST_TESTS:%=direct_%_print)

.PHONY: $(BUILD_SYSTEMS_TESTS:%=build_%_test)
$(BUILD_SYSTEMS_TESTS:%=build_%_test): infer
	$(QUIET)$(call silent_on_success,Running test: $(subst _, ,$@),\
	$(call silence_make,\
	$(MAKE) -C $(INFER_DIR)/tests/build_systems/$(patsubst build_%_test,%,$@) test))

.PHONY: $(BUILD_SYSTEMS_TESTS:%=build_%_print)
$(BUILD_SYSTEMS_TESTS:%=build_%_print): infer
	$(QUIET)$(call silent_on_success,Running: $(subst _, ,$@),\
	$(call silence_make,\
	$(MAKE) -C $(INFER_DIR)/tests/build_systems/$(patsubst build_%_print,%,$@) print))

.PHONY: $(BUILD_SYSTEMS_TESTS:%=build_%_clean)
$(BUILD_SYSTEMS_TESTS:%=build_%_clean):
	$(QUIET)$(call silent_on_success,Cleaning: $(subst _, ,$@),\
	$(call silence_make,\
	$(MAKE) -C $(INFER_DIR)/tests/build_systems/$(patsubst build_%_clean,%,$@) clean))

.PHONY: $(BUILD_SYSTEMS_TESTS:%=build_%_replace)
$(BUILD_SYSTEMS_TESTS:%=build_%_replace): infer
	$(QUIET)$(call silent_on_success,Recording: $(subst _, ,$@),\
	$(call silence_make,\
	$(MAKE) -C $(INFER_DIR)/tests/build_systems/$(patsubst build_%_replace,%,$@) replace))

build_infertop_print build_infertop_test build_infertop_replace: toplevel_test

.PHONY: build_systems_tests
build_systems_tests: $(BUILD_SYSTEMS_TESTS:%=build_%_test)

.PHONY: endtoend_test
endtoend_test: $(BUILD_SYSTEMS_TESTS:%=build_%_test) $(DIRECT_TESTS:%=direct_%_test)

.PHONY: check_missing_mli
check_missing_mli:
	$(QUIET)for x in $$(find $(INFER_DIR)/src -name "*.ml"); do \
	    test -f "$$x"i || echo Missing "$$x"i; done

.PHONY: checkCopyright
checkCopyright: src_build_common
	$(QUIET)$(call silent_on_success,Building checkCopyright,\
	$(MAKE) -C $(SRC_DIR) checkCopyright)

.PHONY: validate-skel
validate-skel:
ifeq ($(IS_FACEBOOK_TREE),yes)
	$(QUIET)$(call silent_on_success,Validating facebook/,\
	$(MAKE) -C facebook validate)
endif

.PHONY: crash_if_not_all_analyzers_enabled
crash_if_not_all_analyzers_enabled:
ifneq ($(BUILD_C_ANALYZERS)+$(BUILD_JAVA_ANALYZERS),yes+yes)
ifneq ($(BUILD_C_ANALYZERS),yes)
	@echo '*** ERROR: Cannot run the full tests: the Clang analyzers are disabled.'
	@echo '*** ERROR: You can run clang-only tests with:'
	@echo '*** ERROR:'
	@echo '*** ERROR:   make config_tests'
	@echo '*** ERROR:'
endif
ifneq ($(BUILD_JAVA_ANALYZERS),yes)
	@echo '*** ERROR: Cannot run the full tests: the Java analyzers are disabled.'
	@echo '*** ERROR: You can run Java-only tests with:'
	@echo '*** ERROR:'
	@echo '*** ERROR:   make config_tests'
	@echo '*** ERROR:'
endif
	@echo '*** ERROR: To run the full set of tests, please enable all the analyzers.'
	@exit 1
else
	@:
endif

.PHONY: mod_dep
mod_dep: src_build_common
	$(QUIET)$(call silent_on_success,Building Infer source dependency graph,\
	$(MAKE) -C $(SRC_DIR) mod_dep.dot)

# `test_build` and `src_build` (which is a dependency of `endtoend_test`) should not be run in
# parallel since they build infer with different profiles (and therefore conflict). Therefore,
# `test_build` is in the dependency, and `endtoend_test` in the recipe.
.PHONY: config_tests
config_tests: test_build ocaml_unit_test validate-skel mod_dep
	$(MAKE) endtoend_test checkCopyright
	$(MAKE) manuals

ifneq ($(filter endtoend_test,$(MAKECMDGOALS)),)
checkCopyright: src_build
toplevel_test: checkCopyright
endif

.PHONY: test
test: crash_if_not_all_analyzers_enabled config_tests
ifeq (,$(findstring s,$(MAKEFLAGS)))
	$(QUIET)echo "$(TERM_INFO)ALL TESTS PASSED$(TERM_RESET)"
endif

.PHONY: test-replace
test-replace: $(BUILD_SYSTEMS_TESTS:%=build_%_replace) $(DIRECT_TESTS:%=direct_%_replace) \
              clang_plugin_test_replace

.PHONY: uninstall
uninstall:
	$(REMOVE_DIR) $(DESTDIR)$(libdir)/infer/
	$(REMOVE) $(DESTDIR)$(bindir)/infer
	$(REMOVE) $(INFER_COMMANDS:%=$(DESTDIR)$(bindir)/%)
	$(REMOVE) $(foreach manual,$(INFER_GROFF_MANUALS_GZIPPED),\
	  $(DESTDIR)$(mandir)/man1/$(notdir $(manual)))
ifeq ($(IS_FACEBOOK_TREE),yes)
	$(MAKE) -C facebook uninstall
endif

.PHONY: test_clean
test_clean: $(DIRECT_TESTS:%=direct_%_clean) $(BUILD_SYSTEMS_TESTS:%=build_%_clean)

.PHONY: install
install: infer $(INFER_GROFF_MANUALS_GZIPPED)
# create directory structure
	test -d      '$(DESTDIR)$(bindir)' || \
	  $(MKDIR_P) '$(DESTDIR)$(bindir)'
	test -d      '$(DESTDIR)$(mandir)/man1' || \
	  $(MKDIR_P) '$(DESTDIR)$(mandir)/man1'
	test -d      '$(DESTDIR)$(libdir)/infer/' || \
	  $(MKDIR_P) '$(DESTDIR)$(libdir)/infer/'
ifeq ($(BUILD_C_ANALYZERS),yes)
	test -d      '$(DESTDIR)$(libdir)/infer/facebook-clang-plugins/libtooling/build/' || \
	  $(MKDIR_P) '$(DESTDIR)$(libdir)/infer/facebook-clang-plugins/libtooling/build/'
	find facebook-clang-plugins/clang/install/. -type d -print0 | xargs -0 -n 1 \
	  $(SHELL) -x -c "test -d '$(DESTDIR)$(libdir)'/infer/\$$1 || \
	    $(MKDIR_P) '$(DESTDIR)$(libdir)'/infer/\$$1" --
	test -d      '$(DESTDIR)$(libdir)/infer/infer/lib/clang_wrappers/' || \
	  $(MKDIR_P) '$(DESTDIR)$(libdir)/infer/infer/lib/clang_wrappers/'
	test -d      '$(DESTDIR)$(libdir)/infer/infer/lib/linter_rules/' || \
	  $(MKDIR_P) '$(DESTDIR)$(libdir)/infer/infer/lib/linter_rules/'
	test -d      '$(DESTDIR)$(libdir)/infer/infer/etc/' || \
	  $(MKDIR_P) '$(DESTDIR)$(libdir)/infer/infer/etc'
endif
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
	test -d      '$(DESTDIR)$(libdir)/infer/infer/lib/java/' || \
	  $(MKDIR_P) '$(DESTDIR)$(libdir)/infer/infer/lib/java/'
endif
	test -d      '$(DESTDIR)$(libdir)/infer/infer/annotations/' || \
	  $(MKDIR_P) '$(DESTDIR)$(libdir)/infer/infer/annotations/'
	test -d      '$(DESTDIR)$(libdir)/infer/infer/lib/wrappers/' || \
	  $(MKDIR_P) '$(DESTDIR)$(libdir)/infer/infer/lib/wrappers/'
	test -d      '$(DESTDIR)$(libdir)/infer/infer/lib/specs/' || \
	  $(MKDIR_P) '$(DESTDIR)$(libdir)/infer/infer/lib/specs/'
	test -d      '$(DESTDIR)$(libdir)/infer/infer/bin/' || \
	  $(MKDIR_P) '$(DESTDIR)$(libdir)/infer/infer/bin/'
# copy files
ifeq ($(BUILD_C_ANALYZERS),yes)
	$(INSTALL_DATA) -C          'facebook-clang-plugins/libtooling/build/FacebookClangPlugin.dylib' \
	  '$(DESTDIR)$(libdir)/infer/facebook-clang-plugins/libtooling/build/FacebookClangPlugin.dylib'
#	do not use "install" for symbolic links as this will copy the destination file instead
	find facebook-clang-plugins/clang/install/. -not -type d -not -type l -not -name '*.a' -print0 \
	  | xargs -0 -I \{\} $(INSTALL_PROGRAM) -C \{\} '$(DESTDIR)$(libdir)'/infer/\{\}
#	all the symlinks in clang are relative and safe to brutally copy over
	find facebook-clang-plugins/clang/install/. -type l -not -name '*.a' -print0 \
	  | xargs -0 -I \{\} $(COPY) -a \{\} '$(DESTDIR)$(libdir)'/infer/\{\}
	find infer/lib/clang_wrappers/* -print0 | xargs -0 -I \{\} \
	  $(INSTALL_PROGRAM) -C \{\} '$(DESTDIR)$(libdir)'/infer/\{\}
#	only for files that point to infer
	(cd '$(DESTDIR)$(libdir)/infer/infer/lib/wrappers/' && \
	 $(foreach cc,$(shell find '$(LIB_DIR)/wrappers' -type l), \
	  [ $(cc) -ef '$(INFER_BIN)' ] && \
	  $(REMOVE) '$(notdir $(cc))' && \
	  $(LN_S) ../../bin/infer '$(notdir $(cc))';))
	find infer/lib/specs/* -print0 | xargs -0 -I \{\} \
	  $(INSTALL_DATA) -C \{\} '$(DESTDIR)$(libdir)'/infer/\{\}
	$(INSTALL_DATA) -C          'infer/lib/linter_rules/linters.al' \
	  '$(DESTDIR)$(libdir)/infer/infer/lib/linter_rules/linters.al'
	$(INSTALL_DATA) -C          'infer/etc/clang_ast.dict' \
	  '$(DESTDIR)$(libdir)/infer/infer/etc/clang_ast.dict'
endif
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
	$(INSTALL_DATA) -C          'infer/annotations/annotations.jar' \
	  '$(DESTDIR)$(libdir)/infer/infer/annotations/annotations.jar'
	find infer/lib/java/*.jar -print0 | xargs -0 -I \{\} \
	  $(INSTALL_DATA) -C \{\} '$(DESTDIR)$(libdir)'/infer/\{\}
	$(INSTALL_PROGRAM) -C      '$(LIB_DIR)'/wrappers/javac \
	  '$(DESTDIR)$(libdir)'/infer/infer/lib/wrappers/
endif
	$(INSTALL_PROGRAM) -C '$(INFER_BIN)' '$(DESTDIR)$(libdir)'/infer/infer/bin/
	(cd '$(DESTDIR)$(bindir)/' && \
	 $(REMOVE) infer && \
	 $(LN_S) '$(libdir_relative_to_bindir)'/infer/infer/bin/infer infer)
	for alias in $(INFER_COMMANDS); do \
	  (cd '$(DESTDIR)$(bindir)'/ && \
	   $(REMOVE) "$$alias" && \
	   $(LN_S) infer "$$alias"); done
	for alias in $(INFER_COMMANDS); do \
	  (cd '$(DESTDIR)$(libdir)'/infer/infer/bin && \
	   $(REMOVE) "$$alias" && \
	   $(LN_S) infer "$$alias"); done
	$(foreach man,$(INFER_GROFF_MANUALS_GZIPPED), \
	  $(INSTALL_DATA) -C $(man) '$(DESTDIR)$(mandir)/man1/$(notdir $(man))';)
ifeq ($(IS_FACEBOOK_TREE),yes)
ifdef DESTDIR
ifeq (,$(findstring :/,:$(DESTDIR)))
#	DESTDIR is set and relative
	$(MAKE) -C facebook install 'DESTDIR=../$(DESTDIR)'
else
#	DESTDIR is set and absolute
	$(MAKE) -C facebook install
endif
else
#	DESTDIR not set
	$(MAKE) -C facebook install
endif
endif

# install dynamic libraries
# use this if you want to distribute infer binaries
install-with-libs: install
	test -d      '$(DESTDIR)$(libdir)'/infer/infer/libso || \
	  $(MKDIR_P) '$(DESTDIR)$(libdir)'/infer/infer/libso
ifneq ($(LDD),no)
ifneq ($(PATCHELF),no)
#	this sort of assumes Linux
#	figure out where libgmp, libmpfr, and libsqlite3 are using ldd
	set -x; \
	for lib in $$($(LDD) $(INFER_BIN) \
	              | cut -d ' ' -f 3 \
	              | grep -e 'lib\(gmp\|mpfr\|sqlite\)'); do \
	  $(INSTALL_PROGRAM) -C "$$lib" '$(DESTDIR)$(libdir)'/infer/infer/libso/; \
	done
#	update rpath of executables
	for sofile in '$(DESTDIR)$(libdir)'/infer/infer/libso/*.so*; do \
	  $(PATCHELF) --set-rpath '$$ORIGIN' --force-rpath "$$sofile"; \
	done
	$(PATCHELF) --set-rpath '$$ORIGIN/../libso' --force-rpath '$(DESTDIR)$(libdir)'/infer/infer/bin/infer
ifeq ($(IS_FACEBOOK_TREE),yes)
	$(PATCHELF) --set-rpath '$$ORIGIN/../libso' --force-rpath '$(DESTDIR)$(libdir)'/infer/infer/bin/InferCreateTraceViewLinks
endif
else # ldd found but not patchelf
	echo "ERROR: ldd (Linux?) found but not patchelf, please install patchelf" >&2; exit 1
endif
else # ldd not found
ifneq ($(OTOOL),no)
ifneq ($(INSTALL_NAME_TOOL),no)
#	this sort of assumes osx
#	figure out where libgmp, libmpfr, and libsqlite3 are using otool
	set -e; \
	set -x; \
	for lib in $$($(OTOOL) -L $(INFER_BIN) \
	              | cut -d ' ' -f 1 | tr -d '\t' \
	              | grep -e 'lib\(gmp\|mpfr\|sqlite\)'); do \
	  $(INSTALL_PROGRAM) -C "$$lib" '$(DESTDIR)$(libdir)'/infer/infer/libso/; \
	done
	set -x; \
	for sofile in '$(DESTDIR)$(libdir)'/infer/infer/libso/*.dylib; do \
	  $(INSTALL_NAME_TOOL) -add_rpath "@executable_path" "$$sofile" 2> /dev/null || true; \
	  scripts/set_libso_path.sh '$(DESTDIR)$(libdir)'/infer/infer/libso "$$sofile"; \
	done
	$(INSTALL_NAME_TOOL) -add_rpath '@executable_path/../libso' '$(DESTDIR)$(libdir)'/infer/infer/bin/infer 2> /dev/null || true
	scripts/set_libso_path.sh '$(DESTDIR)$(libdir)'/infer/infer/libso '$(DESTDIR)$(libdir)'/infer/infer/bin/infer
ifeq ($(IS_FACEBOOK_TREE),yes)
	$(INSTALL_NAME_TOOL) -add_rpath '@executable_path/../libso' '$(DESTDIR)$(libdir)'/infer/infer/bin/InferCreateTraceViewLinks 2> /dev/null || true
	scripts/set_libso_path.sh '$(DESTDIR)$(libdir)'/infer/infer/libso '$(DESTDIR)$(libdir)'/infer/infer/bin/InferCreateTraceViewLinks
endif
else # install_name_tool not found
	echo "ERROR: otool (OSX?) found but not install_name_tool, please install install_name_tool" >&2; exit 1
endif
else # otool not found
	echo "ERROR: need ldd + patchelf (Linux) or otool + install_name_tool (OSX) available" >&2; exit 1
endif
endif # ldd

# Nuke objects built from OCaml. Useful when changing the OCaml compiler, for instance.
.PHONY: ocaml_clean
ocaml_clean:
ifeq ($(BUILD_C_ANALYZERS),yes)
	$(QUIET)$(call silent_on_success,Cleaning facebook-clang-plugins OCaml build,\
	$(MAKE) -C $(FCP_DIR)/clang-ocaml clean)
endif
	$(QUIET)$(call silent_on_success,Cleaning infer OCaml build,\
	$(MAKE) -C $(SRC_DIR) clean)
	$(QUIET)$(call silent_on_success,Cleaning ocamldot,\
	$(MAKE) -C $(DEPENDENCIES_DIR)/ocamldot clean)

.PHONY: clean
clean: ocaml_clean test_clean
ifeq ($(BUILD_C_ANALYZERS),yes)
	$(QUIET)$(call silent_on_success,Cleaning facebook-clang-plugins C++ build,\
	$(MAKE) -C $(FCP_DIR) clean)
endif
	$(QUIET)$(call silent_on_success,Cleaning Java annotations,\
	$(MAKE) -C $(ANNOTATIONS_DIR) clean)
	$(QUIET)$(call silent_on_success,Cleaning infer models,\
	$(MAKE) -C $(MODELS_DIR) clean)
ifeq ($(IS_FACEBOOK_TREE),yes)
	$(QUIET)$(call silent_on_success,Cleaning facebook/,\
	$(MAKE) -C facebook clean)
endif
	$(QUIET)$(call silent_on_success,Removing *.o and *.o.sh,\
	find $(INFER_DIR)/tests \( -name '*.o' -o -name '*.o.sh' \) -delete)
	$(QUIET)$(call silent_on_success,Removing build logs,\
	$(REMOVE_DIR) _build_logs)

.PHONY: conf-clean
conf-clean: clean
	$(REMOVE) .buckversion
	$(REMOVE) Makefile.autoconf
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


# phony because it depends on opam's internal state
.PHONY: opam.locked
opam.locked: opam
# allow users to not force a run of opam update since it's very slow
ifeq ($(NO_OPAM_UPDATE),)
	$(QUIET)$(call silent_on_success,opam update,$(OPAM) update)
endif
	$(QUIET)$(call silent_on_success,generating opam.locked,\
	  $(OPAM) lock .)

OPAM_DEV_DEPS = ocp-indent merlin utop webbrowser

ifneq ($(EMACS),no)
OPAM_DEV_DEPS += tuareg
endif

.PHONY: devsetup
devsetup: Makefile.autoconf
	$(QUIET)[ $(OPAM) != "no" ] || (echo 'No `opam` found, aborting setup.' >&2; exit 1)
	$(QUIET)$(call silent_on_success,installing $(OPAM_DEV_DEPS),\
	  OPAMSWITCH=$(OPAMSWITCH); $(OPAM) install --yes --no-checksum user-setup $(OPAM_DEV_DEPS))
	$(QUIET)echo '$(TERM_INFO)*** Running `opam user-setup`$(TERM_RESET)' >&2
	$(QUIET)OPAMSWITCH=$(OPAMSWITCH); OPAMYES=1; $(OPAM) user-setup install
	$(QUIET)if [ "$(PLATFORM)" = "Darwin" ] && [ x"$(GNU_SED)" = x"no" ]; then \
	  echo '$(TERM_INFO)*** Installing GNU sed$(TERM_RESET)' >&2; \
	  brew install gnu-sed; \
	fi
	$(QUIET)if [ "$(PLATFORM)" = "Darwin" ] && ! $$(parallel -h | grep -q GNU); then \
	  echo '$(TERM_INFO)*** Installing GNU parallel$(TERM_RESET)' >&2; \
	  brew install parallel; \
	fi
	$(QUIET)if [ ! -d "$$HOME"/.parallel ]; then mkdir "$$HOME"/.parallel; fi
	$(QUIET)touch "$$HOME"/.parallel/will-cite
# 	expand all occurrences of "~" in PATH and MANPATH
	$(QUIET)infer_repo_is_in_path=$$(echo $${PATH//\~/$$HOME} | grep -q "$(ABSOLUTE_ROOT_DIR)"/infer/bin; echo $$?); \
	infer_repo_is_in_manpath=$$(echo $${MANPATH//\~/$$HOME} | grep -q "$(ABSOLUTE_ROOT_DIR)"/infer/man; echo $$?); \
	shell_config_file="<could not auto-detect, please fill in yourself>"; \
	if [ $$(basename "$(ORIG_SHELL)") = "bash" ]; then \
	  if [ "$(PLATFORM)" = "Linux" ]; then \
	    shell_config_file="$$HOME"/.bashrc; \
	  else \
	    shell_config_file="$$HOME"/.bash_profile; \
	  fi; \
	elif [ $$(basename "$(ORIG_SHELL)") = "zsh" ]; then \
	  shell_config_file="$$HOME"/.zshrc; \
	fi; \
	if [ "$$infer_repo_is_in_path" != "0" ] || [ "$$infer_repo_is_in_manpath" != "0" ]; then \
	  echo >&2; \
	  echo '$(TERM_INFO)*** NOTE: `infer` is not in your PATH or MANPATH. If you are hacking on infer, you may$(TERM_RESET)' >&2; \
	  echo '$(TERM_INFO)*** NOTE: want to make infer executables and manuals available in your terminal. Type$(TERM_RESET)' >&2; \
	  echo '$(TERM_INFO)*** NOTE: the following commands to configure the current terminal and record the$(TERM_RESET)' >&2; \
	  printf '$(TERM_INFO)*** NOTE: changes in your shell configuration file (%s):$(TERM_RESET)\n' "$$shell_config_file">&2; \
	  echo >&2; \
	  if [ "$$infer_repo_is_in_path" != "0" ]; then \
	    printf '$(TERM_INFO)  export PATH="%s/infer/bin":$$PATH$(TERM_RESET)\n' "$(ABSOLUTE_ROOT_DIR)" >&2; \
	  fi; \
	  if [ "$$infer_repo_is_in_manpath" != "0" ]; then \
	    printf '$(TERM_INFO)  export MANPATH="%s/infer/man":$$MANPATH$(TERM_RESET)\n' "$(ABSOLUTE_ROOT_DIR)" >&2; \
	  fi; \
	  if [ "$$infer_repo_is_in_path" != "0" ]; then \
	    printf "$(TERM_INFO)  echo 'export PATH=\"%s/infer/bin\":\$$PATH' >> \"$$shell_config_file\"$(TERM_RESET)\n" "$(ABSOLUTE_ROOT_DIR)" >&2; \
	  fi; \
	  if [ "$$infer_repo_is_in_manpath" != "0" ]; then \
	    printf "$(TERM_INFO)  echo 'export MANPATH=\"%s/infer/man\":\$$MANPATH' >> \"$$shell_config_file\"$(TERM_RESET)\n" "$(ABSOLUTE_ROOT_DIR)" >&2; \
	  fi; \
	fi; \
	if [ -z "$(ORIG_SHELL_BUILD_MODE)" ]; then \
	  echo >&2; \
	  echo '$(TERM_INFO)*** NOTE: Set `BUILD_MODE=dev` in your shell to disable flambda by default.$(TERM_RESET)' >&2; \
	  echo '$(TERM_INFO)*** NOTE: Compiling with flambda is ~5 times slower than without, so unless you are$(TERM_RESET)' >&2; \
	  echo '$(TERM_INFO)*** NOTE: testing infer on a very large project it will not be worth it. Use the$(TERM_RESET)' >&2; \
	  echo '$(TERM_INFO)*** NOTE: commands below to set the default build mode. You can then use `make opt`$(TERM_RESET)' >&2; \
	  echo '$(TERM_INFO)*** NOTE: when you really do want to enable flambda.$(TERM_RESET)' >&2; \
	  echo >&2; \
	  printf "$(TERM_INFO)  export BUILD_MODE=dev$(TERM_RESET)\n" >&2; \
	  printf "$(TERM_INFO)  echo 'export BUILD_MODE=dev' >> \"$$shell_config_file\"$(TERM_RESET)\n" >&2; \
	fi
	$(QUIET)PATH='$(ORIG_SHELL_PATH)'; if [ "$$(ocamlc -where 2>/dev/null)" != "$$($(OCAMLC) -where)" ]; then \
	  echo >&2; \
	  echo '$(TERM_INFO)*** NOTE: The current shell is not set up for the right opam switch.$(TERM_RESET)' >&2; \
	  echo '$(TERM_INFO)*** NOTE: Please run:$(TERM_RESET)' >&2; \
	  echo >&2; \
	  echo "$(TERM_INFO)  eval \$$($(OPAM) env)$(TERM_RESET)" >&2; \
	fi

GHPAGES ?= no

.PHONY: doc
doc: src_build_common
	$(QUIET)$(call silent_on_success,Generating infer documentation,\
	$(MAKE_SOURCE) doc)
# do not call the browser if we are publishing the docs
ifneq ($(NO_BROWSE_DOC),yes)
	$(QUIET)$(call silent_on_success,Opening in browser,\
	browse $(INFER_DIR)/_build/default/_doc/_html/index.html)
	$(QUIET)echo "Tip: you can generate the doc for all the opam dependencies of infer like this:"
	$(QUIET)echo
	$(QUIET)echo "  odig odoc # takes a while, run it only when the dependencies change"
	$(QUIET)echo "  odig doc"
endif

.PHONY: doc-publish
doc-publish:
ifeq ($(IS_FACEBOOK_TREE),yes)
	$(QUIET)$(call silent_on_success,Cleaning up FB-only files,\
	$(MAKE) -C $(SRC_DIR) clean; \
	$(MAKE) -C facebook clean)
endif
	$(QUIET)$(call silent_on_success,Building infer and manuals,\
	$(MAKE) $(INFER_GROFF_MANUALS))
	$(QUIET)$(MKDIR_P) "$(WEBSITE_DIR)"/static/man/next "$(WEBSITE_DIR)"/static/odoc/next
	$(QUIET)$(call silent_on_success,Copying man pages,\
	$(REMOVE) "$(WEBSITE_DIR)"/static/man/*; \
	for man in $(INFER_GROFF_MANUALS); do \
	  groff -Thtml "$$man" > "$(WEBSITE_DIR)"/static/man/next/$$(basename "$$man").html; \
	done)
	$(QUIET)$(call silent_on_success,Building OCaml modules documentation,\
	$(MAKE) IS_FACEBOOK_TREE=no NO_BROWSE_DOC=yes doc)
	$(QUIET)$(call silent_on_success,Copying OCaml modules documentation,\
	rsync -a --delete $(BUILD_DIR)/default/_doc/_html/ "$(WEBSITE_DIR)"/static/odoc/next/)
	$(QUIET)$(call silent_on_success,Building infer,\
	$(MAKE) src_build)
	$(QUIET)$(call silent_on_success,Calling 'infer help --write-website',\
	$(INFER_BIN) help --write-website "$(WEBSITE_DIR)")

# print list of targets
.PHONY: show-targets
show-targets:
	$(QUIET)$(MAKE) -pqrR . | grep --only-matching -e '^[a-zA-Z0-9][^ ]*:' | cut -d ':' -f 1 | sort
