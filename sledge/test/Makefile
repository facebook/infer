# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# additional arguments to pass to clang
CLANG_ARGS?=-O0

# executable to test
SLEDGE_EXE=$(CURDIR)/../_build/_install/dbg/bin/sledge

# additional arguments to pass to sledge
SLEDGE_ARGS?=

# limits for each test run
TIMEOUT?=30
MEMOUT?=4096

SLEDGE=./wrap.sh $(TIMEOUT) $(MEMOUT) $(SLEDGE_EXE)

DIFF?=diff

# configure the non-host llvm and clang
export PATH := $(CURDIR)/../.llvm_install/sledge/bin:$(PATH)

default: test

# all analyze tests
translate:
	@find -L llvm -name '*.ll' -or -name '*.bc' \
	 | parallel --bar $(SLEDGE) llvm translate -no-models -no-internalize $(SLEDGE_ARGS)

_translate-report-raw:
	@find -L llvm -name '*.out' \
	 | xargs grep "RESULT:" \
	 | sed 's/\.out:/:	/' | sort | sort -s -t':' -k 3,4

# report all results
translate-report-full:
	@$(MAKE) --silent _translate-report-raw | column -ts$$'\t'

# report all errors
translate-report-errors:
	@$(MAKE) --silent _translate-report-raw \
	 | grep -E -v "RESULT: (Success|Invalid input)" \
	 | column -ts$$'\t'

# report errors
translate-report:
	@$(MAKE) --silent _translate-report-raw \
	 | grep -E -v "RESULT: Unimplemented" \
	 | grep -E -v "RESULT: (Success|Invalid input)" \
	 | column -ts$$'\t'

Buckets:=\
  "Invalid input"\
  "Success"\
  "Unimplemented: ConstantAggregateZero of size 0"\
  "Unimplemented: call null"\
  "Unimplemented: coroutines"\
  "Unimplemented: ifuncs"\
  "Unimplemented: inline asm"\
  "Unimplemented: landingpad of type"\
  "Unimplemented: non-integral pointer types"\
  "Unimplemented: opcode kind in call instruction"\
  "Unimplemented: operand kind in call instruction"\
  "Unimplemented: size_of"\
  "Unimplemented: statepoints"\
  "Unimplemented: unsized non-opaque aggregate types"\
  "Unimplemented: vector operations"\
  "Unimplemented: windows exception handling"\
  "segmentation violation"

report-summary:
	for b in $(Buckets); do (printf "$$b\t"; grep -E "$$b" report.expected | wc -l); done | sort -t$$'\t' -k 2 -n -r | column -ts$$'\t'

# compile c to llvm bitcode
%.bc : %.c
	@(cd $(dir $*) && clang -g -c -emit-llvm $(CLANG_ARGS) $(notdir $*).c -o $(notdir $*).bc)

# compile c++ to llvm bitcode
%.bc : %.cpp
	@(cd $(dir $*) && clang++ -g -c -emit-llvm $(CLANG_ARGS) $(notdir $*).cpp -o $(notdir $*).bc)

# code to test analyze
AnalyzeCs:=$(shell find * -not -path 'llvm/*' -name '*.c')
AnalyzeCPPs:=$(shell find * -not -path 'llvm/*' -name '*.cpp')

AnalyzeBCs:=$(patsubst %.c,%.bc,$(AnalyzeCs)) $(patsubst %.cpp,%.bc,$(AnalyzeCPPs))
AnalyzeLLs:=$(shell find * -not -path 'llvm/*' -name '*.ll')

AnalyzeTests:=$(AnalyzeBCs) $(AnalyzeLLs)

# compile all c/c++ to bc
compile: $(AnalyzeBCs)

# all analyze tests
analyze: compile
	@parallel --bar $(SLEDGE) llvm analyze $(SLEDGE_ARGS) ::: $(AnalyzeTests)

# run all tests and generate code coverage information
BISECT_DIR=$(CURDIR)/../_coverage/out
coverage:
	@cd ..; dune build _build/_install/coverage/bin/sledge
	@mkdir -p $(BISECT_DIR)
	@-$(MAKE) BISECT_FILE=$(BISECT_DIR)/bisect SLEDGE_EXE=$(CURDIR)/../_build/_install/coverage/bin/sledge test -k
	@find $(BISECT_DIR) -type f | xargs bisect-ppx-report -I ../_build/_install/coverage/ -text ../_coverage/summary.txt -html ../_coverage/
	@echo "open ../_coverage/index.html"

_analyze-report-raw:
	@find * -not -path 'llvm/*' -name '*.out' \
	 | xargs grep "RESULT:" \
	 | sed 's/\.out:/:	/' | sort | sort -s -t':' -k 3,4

# report all results
analyze-report-full:
	@$(MAKE) --silent _analyze-report-raw | column -ts$$'\t'

# list tests with zero or multiple RESULT lines
report-invalid-results:
	@find -L * -name '*.out' -exec grep -H -c "RESULT:" {} \; \
	 | grep -v ":1$"

# report warnings
warnings:
	@find -L * -name '*.out' | xargs grep -h "Warning:" | sort

# run tests and check against expected results
test:
	-@$(MAKE) --silent --keep-going clean analyze translate 2>/dev/null
	@$(MAKE) --silent _analyze-report-raw > report.current
	@$(MAKE) --silent _translate-report-raw >> report.current
	@$(DIFF) report.expected report.current

# set current results as new expected results
promote:
	@cp report.current report.expected

# remove generated bitcode files
cleanbc:
	@rm -f $(AnalyzeBCs)

# remove result files
cleanout:
	@find -L * -name "*.out" -or -name '*.err' -or -name '*.llair' \
	 | xargs rm -f

clean: cleanbc cleanout

fmt:
	clang-format -i $(AnalyzeCs) $(AnalyzeCPPs)

# print any variable for Makefile debugging
print-%:
	@printf '$*='; printf '$($*)'; printf '\n'
