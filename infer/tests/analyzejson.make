# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Makefiles that include this one must define TESTS_DIR and then include
# $(TESTS_DIR)/analyzejson.make.

include $(TESTS_DIR)/infer.make

infer-out/report.json: $(MAKEFILE_LIST)
	$(QUIET)$(call silent_on_success,Testing infer/dotnet in $(TEST_REL_DIR),\
      $(INFER_BIN) capture && \
	  $(INFER_BIN) analyzejson \
	    $(INFER_OPTIONS) )