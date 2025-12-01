# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR=../../..

INFER_OPTIONS =

SOURCES = $(sort $(wildcard *.rs.ullbc))
CAPTURE_CMD = $(patsubst %.rs.ullbc, --capture-rust-ullbc %.rs.ullbc,$(SOURCES))
RUSTSOURCES = $(realpath $(wildcard *.rs))

include $(TESTS_DIR)/base.make

.NOTPARALLEL:

default: test

# Print_ullbc and ullbc genaration is tested only when the rust analyzer is enabled
%.print_ullbc: %.rs
	$(QUIET)if [ "$(BUILD_RUST_ANALYZERS)" = "yes" ]; then \
	error=0; for file in $(RUSTSOURCES) ; do \
		charon rustc --print-ullbc --ullbc --dest-file /dev/null -- $$file --allow unused > $$file.test.print_ullbc || error=1 ; \
	done; fi

%.ullbc: %.rs
	$(QUIET)if [ "$(BUILD_RUST_ANALYZERS)" = "yes" ]; then \
	error=0; for file in $(RUSTSOURCES) ; do \
		charon rustc --ullbc --dest-file $$file.test.ullbc -- $$file --allow unused || error=1 ; \
	done; fi

%.test.sil: %.ullbc
	$(QUIET)$(call silent_on_success,Testing the infer/rust frontend in $(TEST_REL_DIR),\
	  $(INFER_BIN) capture --frontend-tests --dump-textual --project-root $(TESTS_DIR) $(CAPTURE_CMD))

capture: $(SOURCES:.ullbc=.test.sil)
ullbc: $(RUSTSOURCES:.rs=.ullbc)
print_ullbc: $(RUSTSOURCES:.rs=.print_ullbc)

.PHONY: print
print: capture

.PHONY: test
test: ullbc print_ullbc capture
	$(QUIET)error=0; for file in $(SOURCES) ; do \
	  diff -u "$$file.sil" "$$file.test.sil" || error=1 ; \
	done ; \
	if [ "$(BUILD_RUST_ANALYZERS)" = "yes" ]; then \
	error=0; for file in $(RUSTSOURCES) ; do \
		diff -u "$$file.ullbc" "$$file.test.ullbc" || error=1 ; \
		diff -u "$$file.print_ullbc" "$$file.test.print_ullbc" || error=1 ; \
	done; fi; \
	if [ $$error = 1 ]; then exit 1; fi

.PHONY: replace
replace: ullbc print_ullbc capture
	$(QUIET)for file in $(SOURCES) ; do \
	  mv $$file.test.sil $$file.sil ; \
	done; \
	if [ "$(BUILD_RUST_ANALYZERS)" = "yes" ]; then \
	error=0; for file in $(RUSTSOURCES) ; do \
		mv "$$file.test.ullbc" "$$file.ullbc" || error=1 ; \
		mv "$$file.test.print_ullbc" "$$file.print_ullbc" || error=1 ; \
	done; fi

.PHONY: clean
clean:
	$(REMOVE_DIR) infer-out *.test.sil *.test.ullbc *.test.print_ullbc *.test.dot $(CLEAN_EXTRA)

# (Re)generate the ullbc and print_ullbc files for all rust programs in the folder
.PHONY: charon
charon:
	$(QUIET)if [ "$(BUILD_RUST_ANALYZERS)" = "yes" ]; then \
	error=0; for file in $(RUSTSOURCES) ; do \
		charon rustc --print-ullbc --ullbc --dest-file $$file.ullbc -- $$file --allow unused > $$file.print_ullbc || error=1 ; \
	done; fi
