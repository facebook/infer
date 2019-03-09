# Copyright (c) 2018-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# additional arguments to pass to clang
OPT_ARGS?=-Os
CLANG_ARGS?=-g $(OPT_ARGS)

# additional arguments to pass to sledge
SLEDGE_ARGS?=

# limits for each test run
TIMEOUT?=30
MEMOUT?=4096
MEMOUTk=$(shell echo $$(( $(MEMOUT) * 1024 )))

# executable to test
SLEDGE_EXE=$(CURDIR)/../_build/dev/src/sledge.exe

MODEL_DIR=$(CURDIR)/../model

# configure the non-host llvm and clang
export PATH := $(CURDIR)/../_opam/llvm/bin:$(PATH)

# code to test
SrcCs:=$(shell find -L * -not -path 'llvm/*' -name '*.c')
SrcCPPs:=$(shell find -L * -not -path 'llvm/*' -name '*.cpp')
SrcLLs:=$(shell find -L * -name '*.ll')
SrcBCs:=$(shell find -L * -name '*.bc')

GenBCs:=$(patsubst %.c,%.bc,$(SrcCs)) $(patsubst %.cpp,%.bc,$(SrcCPPs))

TestBCs:=$(GenBCs) $(SrcBCs)
TestLLs:=$(SrcLLs)
Tests:=$(TestBCs) $(TestLLs)
Outs:=$(patsubst %.bc,%.bc.out,$(TestBCs)) $(patsubst %.ll,%.ll.out,$(TestLLs))

default: test

$(MODEL_DIR)/cxxabi.bc : $(MODEL_DIR)/cxxabi.cpp
	@(cd $(dir $@) && clang $(CLANG_ARGS) -I../llvm/projects/libcxxabi/include -I../llvm/projects/libcxxabi/src -c -emit-llvm cxxabi.cpp)

# compile c to llvm bitcode
%.bc : %.c $(MODEL_DIR)/cxxabi.bc
	@(cd $(dir $*) && clang $(CLANG_ARGS) -c -emit-llvm $(notdir $*).c -o - | opt $(OPT_ARGS) -o $(notdir $*).bc)

#  $(MODEL_DIR)/cxxabi.bc
#	@(cd $(dir $*) && clang $(CLANG_ARGS) -c -emit-llvm $(notdir $*).c -o - | llvm-link $(MODEL_DIR)/cxxabi.bc - | opt $(OPT_ARGS) -o $(notdir $*).bc)

# compile c++ to llvm bitcode
%.bc : %.cpp
	@(cd $(dir $*) && clang $(CLANG_ARGS) -c -emit-llvm $(notdir $*).cpp -o - | opt $(OPT_ARGS) -o $(notdir $*).bc)

#  $(MODEL_DIR)/cxxabi.bc
#	@(cd $(dir $*) && clang $(CLANG_ARGS) -c -emit-llvm $(notdir $*).cpp -o - | llvm-link $(MODEL_DIR)/cxxabi.bc - | opt $(OPT_ARGS) -o $(notdir $*).bc)

# disassemble bitcode to llvm assembly
%.ll : %.bc
	@(cd $(dir $*) && llvm-dis -show-annotations -o $(notdir $*).ll $(notdir $*).bc)

# analyze one test input
define analyze
  @bash -c ' \
    if test -e $(1).$(2); then \
    cd $(dir $(1)); \
    status=$$( \
        ( ulimit -t $(TIMEOUT) -v $(MEMOUTk) \
        ; $(SLEDGE_EXE) $(SLEDGE_ARGS) $(notdir $(1).$(2)) 1> $(notdir $(1).$(2)).out 2> $(notdir $(1).$(2)).err ) \
    )$$?; \
    case $$status in \
      ( 0 | 2 ) ;; \
      ( 152 ) echo -e "RESULT: TIMEOUT" >> $(notdir $(1).$(2)).out ;; \
      ( * ) echo -e "\nRESULT: Internal error: "$$status >> $(notdir $(1).$(2)).out ;; \
    esac ; \
    exit $$status ; \
    fi; '
endef

# analyze ll
%.ll.out : %.ll
	$(call analyze,$*,ll)

# analyze bc
%.bc.out : %.bc
	$(call analyze,$*,bc)

# compile all c to bc
compile: $(GenBCs)

# run all tests
test: $(Outs)

# run all tests and generate code coverage information
BISECT_DIR=$(CURDIR)/../_coverage/out
coverage:
	@cd ..; dune build _build/coverage/src/sledge.exe
	@mkdir -p $(BISECT_DIR)
	@-$(MAKE) BISECT_FILE=$(BISECT_DIR)/bisect SLEDGE_EXE=$(CURDIR)/../_build/coverage/src/sledge.exe test -k
	@find $(BISECT_DIR) -type f | xargs bisect-ppx-report -I ../_build/coverage/ -text ../_coverage/summary.txt -html ../_coverage/
	@echo "open ../_coverage/index.html"

# translate all tests
translate:
	$(MAKE) SLEDGE_ARGS='$(SLEDGE_ARGS) --compile-only' test

# report all results
full-report:
	@find -L * -name '*.out' \
	 | xargs grep "RESULT:" \
	 | sed 's/.out:/:	/' | column -ts$$'\t' | sort | sort -s -t':' -k 3,4  | uniq

# report all errors
report-errors:
	@find -L * -name '*.out' \
	 | xargs grep "RESULT:" \
	 | grep -E -v "RESULT: (Success|Invalid input)" \
	 | sed 's/.out:/:	/' | column -ts$$'\t' | sort | sort -s -t':' -k 3,4 | uniq

# report errors
report:
	@find -L * -name '*.out' \
	 | xargs grep "RESULT:" \
	 | grep -E -v "RESULT: Unimplemented: (landingpad of type other than {i8\*, i32}|windows exception handling|non-integral pointer types|types with undetermined size):" \
	 | grep -E -v "RESULT: (Success|Invalid input)" \
	 | sed 's/.out:/:	/' | column -ts$$'\t' | sort | sort -s -t':' -k 3,4 | uniq

# report warnings
warnings:
	@find -L * -name '*.out' | xargs grep -h "Warning:" | sort | uniq

# remove generated bitcode files
cleanBC:
	@rm -f $(GenBCs)

# remove result files
cleanout:
	@find -L * -name "*.out" -or -name '*.err' \
	 | xargs rm -f

# remove result files for TIMEOUTs
cleanTO:
	@find -L * -name "*.out" -or -name '*.err' \
	 | xargs grep -l TIMEOUT \
	 | xargs rm -f

# remove result files for MEMOUTs
cleanMO:
	@find -L * -name "*.out" -or -name '*.err' \
	 | xargs grep -l MEMOUT \
	 | xargs rm -f

# remove result files for Internal Errors
cleanIE:
	@find -L * -name "*.out" -or -name '*.err' \
	 | xargs grep -l -E "RESULT: (Internal error|Unimplemented|Unknown error)" \
	 | xargs rm -f

clean: cleanBC cleanout

fmt:
	clang-format -i $(SrcCs) $(SrcCPPs)

# print any variable for Makefile debugging
print-%:
	@printf '$*='; printf '$($*)'; printf '\n'
