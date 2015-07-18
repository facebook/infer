# Copyright (c) 2004 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.
BD=~/clang-llvm/build

all:
	/usr/local/bin/clang++ -DGNU_SOURCE -D_DEBUG -D_STDC_CONSTANT_MACROS \
	        -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS \
					-I$(BD)/tools/clang/include -I$(BD)/lib/clang/3.4/include -I$(BD)/include \
					-Xclang -load -Xclang $(BD)/lib/infer-plugin.so \
					-Xclang -plugin -Xclang infer-plugin \
					test.cpp -fsyntax-only

