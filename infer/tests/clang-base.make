# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

%.o: %.c
	clang $(CLANG_OPTIONS) -o $@ $<

%.o: %.cpp
	clang++ $(CLANG_OPTIONS) -o $@ $<

%.o: %.m
	clang $(CLANG_OPTIONS) -o $@ $<

%.o: %.mm
	clang++ $(CLANG_OPTIONS) -o $@ $<
