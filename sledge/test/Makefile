# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# additional arguments to pass to clang
CLANG_ARGS?=-O0

# executable to test
SLEDGE_DBG=$(CURDIR)/../_build/debug/cli/sledge_cli.exe
SLEDGE_OPT=$(CURDIR)/../_build/release/cli/sledge_cli.exe

# additional arguments to pass to sledge
SLEDGE_ARGS?=

# limits for each test run
TIMEOUT?=90
MEMOUT?=4096

sledge_dbg=./wrap.sh $(TIMEOUT) $(MEMOUT) $(SLEDGE_DBG)
sledge_opt=./wrap.sh $(TIMEOUT) $(MEMOUT) $(SLEDGE_OPT)

sledge_report=$(CURDIR)/../_build/debug/report/sledge_report.exe

# which utilities to use
diff?=patdiff

# configure the non-host llvm and clang
export PATH := $(CURDIR)/../.llvm_install/sledge/bin:$(PATH)

# configure sort
export LANG := C

default: test

# compile c to llvm bitcode
%.bc : %.c
	@(cd $(dir $*) && clang -g -c -emit-llvm -include $(CURDIR)/../model/llair_intrinsics.h -Wno-main-return-type $(CLANG_ARGS) $(notdir $*).c -o $(notdir $*).bc)

# compile c++ to llvm bitcode
%.bc : %.cpp
	@(cd $(dir $*) && clang++ -g -c -emit-llvm $(CLANG_ARGS) $(notdir $*).cpp -o $(notdir $*).bc)

%.llair : %.bc
	$(SLEDGE_DBG) llvm translate $< -llair-output $@

%.llair.txt : %.llair
	$(SLEDGE_DBG) disassemble $< -llair-txt-output $@

# code to test sledge translate
TranslateCs:=$(shell find -L {,local/}translate -name '*.c' 2>/dev/null)
TranslateCPPs:=$(shell find -L {,local/}translate -name '*.cpp' 2>/dev/null)
TranslateGenBCs:=$(patsubst %.c,%.bc,$(TranslateCs)) $(patsubst %.cpp,%.bc,$(TranslateCPPs))
TranslateBCs:=$(TranslateGenBCs) $(shell find -L extra/translate -name '*.bc' 2>/dev/null)
TranslateLLs:=$(shell find -L {,local/}translate -name '*.ll' 2>/dev/null)
TranslateTests:=$(TranslateBCs) $(TranslateLLs)

# code to test sledge analyze
AnalyzeCs:=$(shell find -L {,local/}analyze -name '*.c' 2>/dev/null)
AnalyzeCPPs:=$(shell find -L {,local/}analyze -name '*.cpp' 2>/dev/null)
AnalyzeGenBCs:=$(patsubst %.c,%.bc,$(AnalyzeCs)) $(patsubst %.cpp,%.bc,$(AnalyzeCPPs))
AnalyzeBCs:=$(AnalyzeGenBCs) $(shell find -L extra/analyze -name '*.bc' 2>/dev/null)
AnalyzeLLs:=$(shell find -L {,local/}analyze -name '*.ll' 2>/dev/null)
AnalyzeTests:=$(AnalyzeBCs) $(AnalyzeLLs)

GeneratedBCs:=$(TranslateGenBCs) $(AnalyzeGenBCs)

# compile all c/c++ to bc
compile: $(GeneratedBCs)
	@if [ -d "extra" ]; then \
	   $(MAKE) sledge=$(SLEDGE_DBG) -C extra compile; \
	 fi

#
# translation (frontend) tests
#

# run sledge llvm translate tests
.PHONY: translate
translate: compile
	-@parallel --bar $(sledge_dbg) llvm disassemble -no-models -no-internalize $(SLEDGE_ARGS) ::: $(TranslateTests)

.PHONY: translate.sexp
translate.sexp:
	@find -L {,local/,extra/}translate -not -path 'baseline/*' -name '*.sexp' 2>/dev/null \
	 | xargs cat > translate.sexp

baseline/translate.sexp: translate.sexp
	@cp translate.sexp $@

translate.html: translate.sexp
	@$(sledge_report) html -baseline baseline/translate.sexp translate.sexp -output $@

translate-status: translate.sexp
	@$(sledge_report) status -baseline baseline/translate.sexp translate.sexp | column -ts$$'\t'

#
# analyze (backend) tests
#

# run analyze tests
.PHONY: analyze
analyze: compile
	-@parallel --bar $(sledge_dbg) llvm analyze -bound 5 -trace Report+Control $(SLEDGE_ARGS) ::: $(AnalyzeTests)

parallel_sledge_opt=parallel --shuf --bar $(sledge_opt) llvm analyze -append-report -bound 5 $(SLEDGE_ARGS) :::

analyze-perf1: compile
	@$(parallel_sledge_opt) $(AnalyzeTests)

analyze-perf3: compile
	@$(parallel_sledge_opt) $(AnalyzeTests) $(AnalyzeTests) $(AnalyzeTests)

analyze-perf5: compile
	@$(parallel_sledge_opt) $(AnalyzeTests) $(AnalyzeTests) $(AnalyzeTests) $(AnalyzeTests) $(AnalyzeTests)

analyze-perf7: compile
	@$(parallel_sledge_opt) $(AnalyzeTests) $(AnalyzeTests) $(AnalyzeTests) $(AnalyzeTests) $(AnalyzeTests) $(AnalyzeTests) $(AnalyzeTests)

.PHONY: analyze.sexp
analyze.sexp:
	@find -L {,local/,extra/}analyze -not -path 'baseline/*' -name '*.sexp' 2>/dev/null \
	 | xargs cat > analyze.sexp

baseline/analyze.sexp: analyze.sexp
	@cp analyze.sexp $@

analyze.html: analyze.sexp
	@$(sledge_report) html -baseline baseline/analyze.sexp analyze.sexp -output $@

analyze-status: analyze.sexp
	@$(sledge_report) status -baseline baseline/analyze.sexp analyze.sexp | column -ts$$'\t'

#
# smt tests
#

SmtTests:=$(shell find smt -name '*.smt2' 2>/dev/null)

# run sledge smt tests
.PHONY: smt
smt:
	-@parallel --bar $(sledge_dbg) smt $(SLEDGE_ARGS) ::: $(SmtTests)

parallel_sledge_opt_smt=parallel --shuf --bar $(sledge_opt) smt -append-report $(SLEDGE_ARGS) :::

smt-perf1:
	@$(parallel_sledge_opt_smt) $(SmtTests)

smt-perf3:
	@$(parallel_sledge_opt_smt) $(SmtTests) $(SmtTests) $(SmtTests)

smt-perf5:
	@$(parallel_sledge_opt_smt) $(SmtTests) $(SmtTests) $(SmtTests) $(SmtTests) $(SmtTests)

smt-perf7:
	@$(parallel_sledge_opt_smt) $(SmtTests) $(SmtTests) $(SmtTests) $(SmtTests) $(SmtTests) $(SmtTests) $(SmtTests)

.PHONY: smt.sexp
smt.sexp:
	@find -L {,local/,extra/}smt -not -path 'baseline/*' -name '*.sexp' 2>/dev/null \
	 | xargs cat > smt.sexp

baseline/smt.sexp: smt.sexp
	@cp smt.sexp $@

smt.html: smt.sexp
	@$(sledge_report) html -baseline baseline/smt.sexp smt.sexp -output $@

smt-status: smt.sexp
	@$(sledge_report) status -baseline baseline/smt.sexp smt.sexp | column -ts$$'\t'

#
# llvm tests
#

# run sledge llvm translate tests
.PHONY: llvm
llvm:
	-@find -L llvm -name '*.ll' -or -name '*.bc' 2>/dev/null \
	 | parallel --bar $(sledge_dbg) llvm translate -no-models -no-internalize $(SLEDGE_ARGS)

.PHONY: llvm.sexp
llvm.sexp:
	@find -L {,local/,extra/}llvm -not -path 'baseline/*' -name '*.sexp' 2>/dev/null \
	 | xargs cat > llvm.sexp

baseline/llvm.sexp: llvm.sexp
	@cp llvm.sexp $@

llvm.html: llvm.sexp
	@$(sledge_report) html -baseline baseline/llvm.sexp llvm.sexp -output $@

llvm-status: llvm.sexp
	@$(sledge_report) status -baseline baseline/llvm.sexp llvm.sexp | column -ts$$'\t'

#
# test workflow
#

# report warnings
warnings:
	@find -L * -name '*.out' | xargs grep -h "Warning:" | sort

test-translate:
	-@find -L {,local/,extra/}translate -name "*.sexp" -or -name "*.out" -or -name '*.err' -or -name '*.llair' -or -name '*.llair.txt' -delete 2>/dev/null
	-@$(MAKE) --no-print-directory translate
	@$(MAKE) --no-print-directory translate-status

test-analyze:
	-@find -L {,local/,extra/}analyze -name "*.sexp" -or -name "*.out" -or -name '*.err' -delete 2>/dev/null
	-@$(MAKE) --no-print-directory analyze
	@$(MAKE) --no-print-directory analyze-status

test-smt:
	-@find -L {,local/,extra/}smt -name "*.sexp" -or -name "*.out" -or -name '*.err' -delete 2>/dev/null
	-@$(MAKE) --no-print-directory smt
	@$(MAKE) --no-print-directory smt-status

test-llvm:
	-@find -L {,local/,extra/}llvm -name "*.sexp" -or -name "*.out" -or -name '*.err' -or -name '*.llair' -delete 2>/dev/null
	-@$(MAKE) --no-print-directory llvm
	@$(MAKE) --no-print-directory llvm-status

# run tests and check against expected results
test: test-translate test-analyze test-smt test-llvm

# set current results as new expected results
promote: baseline/translate.sexp baseline/analyze.sexp baseline/smt.sexp baseline/llvm.sexp

# copy the current out and err files to the baseline dir
promote-out:
	@find -L * -not -path 'baseline/*' \( -name '*.out' -or -name '*.err' \) | xargs gcp --parents -t baseline

# diff the current out files against the ones in the baseline dir
diff-out:
	@$(diff) -mask-uniques -include '.*\.out' baseline -alt-next current .

# diff the current err files against the ones in the baseline dir
diff-err:
	@$(diff) -mask-uniques -include '.*\.err' baseline -alt-next current .

# remove generated bitcode files
cleanbc:
	@rm -f $(GeneratedBCs)

# remove result files
cleanout:
	@find -L * -not -path 'baseline/*' -name "*.sexp" -or -name "*.out" -or -name '*.err' -or -name '*.llair' -or -name '*.llair.txt' \
	 | xargs rm -f

clean: cleanbc cleanout

fmt:
	clang-format -i $(AnalyzeCs) $(AnalyzeCPPs)

# print any variable for Makefile debugging
print-%:
	@printf '$*='; printf '$($*)'; printf '\n'
