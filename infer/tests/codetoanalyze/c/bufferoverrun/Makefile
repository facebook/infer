# Copyright (c) 2016 - present
#
# Programming Research Laboratory (ROPAS)
# Seoul National University, Korea
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../../..

ANALYZER = bufferoverrun
CLANG_OPTIONS = -c
INFER_OPTIONS = -nf --project-root $(TESTS_DIR) --no-failures-allowed
INFERPRINT_OPTIONS = --issues-tests

SOURCES = \
  trivial.c \
  function_call.c \
  for_loop.c \
  while_loop.c \
  do_while.c \
  nested_loop.c \
  nested_loop_with_label.c \
  break_continue_return.c \
  goto_loop.c \
  inf_loop.c

include $(TESTS_DIR)/clang.make
