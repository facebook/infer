# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..
ROOT_DIR = $(TESTS_DIR)/../..
include $(ROOT_DIR)/Makefile.config

define check_no_diff
  git --no-pager diff --color=auto --no-ext-diff --no-index --word-diff --unified=1 --minimal \
	$$(realpath $(1)) $$(realpath $(2)) >&2 || \
  (printf '\n' >&2; \
   printf 'Eval output (with infer) differs from expected eval output (with python3)\n' >&2; \
   exit 1)
endef

default: test

show_print_output: main_test.py module1.py
	python3 script_show_print_output.py main_test.py

eval_with_python.output: main_test.py module1.py show_print_output
	python3 main_test.py > eval_with_python.output

eval_with_infer.output: $(INFER_BIN) main_test.py module1.py show_print_output
	$(INFER_BIN) --run-python-interpreter -- python3 main_test.py module1.py > eval_with_infer.output

.PHONY: test
test: eval_with_python.output eval_with_infer.output
	$(call check_no_diff,eval_with_python.output,eval_with_infer.output)

.PHONY: replace
replace:

.PHONY: clean
clean:
	$(REMOVE_DIR) eval_with_python.output eval_with_infer.output main_test.py.temp
