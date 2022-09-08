# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Makefiles that include this one must define TESTS_DIR and then include
# $(TESTS_DIR)/analyzejson.make.

include $(TESTS_DIR)/infer.make

cfg.json: jsons.tar.xz
	$(QUIET)tar xf $<

infer-out/report.json: $(MAKEFILE_LIST) cfg.json
	$(QUIET)$(call silent_on_success,Testing infer/dotnet in $(TEST_REL_DIR),\
	  $(INFER_BIN)   $(infer help --list-issue-types 2> /dev/null | grep ':true:' | cut -d ':' -f 1 | sed -e 's/^/--disable-issue-type /') --enable-issue-type NULL_DEREFERENCE --enable-issue-type DOTNET_RESOURCE_LEAK --enable-issue-type THREAD_SAFETY_VIOLATION run --project-root $(TESTS_DIR) \
	    $(INFER_OPTIONS) && \
	  sed -i -e 's#/app/infernew/infer/tests/##g' $@ )
