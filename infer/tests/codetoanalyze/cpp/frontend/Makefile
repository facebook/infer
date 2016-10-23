# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

include ../../Makefile.frontend

OPTIONS = -x c++ -std=c++11 -isystem$(ROOT_DIR) -isystem$(CLANG_INCLUDES)/c++/v1/ -c

FILES = \
  */*.cpp shared/*/*.cpp \

compile:
	clang $(OPTIONS) $(FILES)

capture:
	$(INFER_BIN) -a capture --frontend-tests --cxx --headers -- clang $(OPTIONS) $(FILES)
