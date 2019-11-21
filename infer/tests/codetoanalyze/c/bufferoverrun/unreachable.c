/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

int __infer_nondet_int() {
  int ret;
  return ret;
}

void nop() {}

void condition_always_true_bad() {
  if (1) {
    nop();
  }
}

void condition_always_false_bad() {
  if (0) {
    nop();
  }
}

void condition_always_true_with_else_bad() {
  if (1) {
    nop();
  } else {
    nop();
  }
}

void exit_at_end_of_if_good() {
  if (__infer_nondet_int()) {
    exit(4);
  }
}

void exit_at_end_of_proc_good() {
  nop();
  exit(5);
}

void exit_at_end_of_proc_good_local_var() {
  int a = 57;
  exit(5);
}

void FN_useless_else_bad() {
  if (__infer_nondet_int()) {
    exit(0);
  } else {
    nop();
  }
}

void never_loops_bad() {
  while (0) {
    nop();
  }
}

void infinite_loop_bad() {
  while (1) {
    nop();
  }
}

void FP_loop_with_break_good() {
  while (1) {
    if (__infer_nondet_int()) {
      break;
    }
  }
}

void FP_loop_with_return_good() {
  while (1) {
    if (__infer_nondet_int()) {
      return;
    }
  }
}

void FP_loop_with_exit_good() {
  while (1) {
    if (__infer_nondet_int()) {
      exit(1);
    }
  }
}

void FP_loop_with_unreachable_good() {
  while (1) {
    if (__infer_nondet_int()) {
      infinite_loop_bad();
    }
  }
}

void FP_loop_once_intentional_good() {
  do {
    nop();
  } while (0);
}

void FN_loop_once_break_bad() {
  while (__infer_nondet_int()) {
    break;
  }
}

void FN_loop_once_return_bad() {
  while (__infer_nondet_int()) {
    return;
  }
}

void FN_loop_once_exit_bad() {
  while (__infer_nondet_int()) {
    exit(2);
  }
}

void FN_loop_once_unreachable_bad() {
  while (__infer_nondet_int()) {
    infinite_loop_bad();
  }
}

void FN_unreachable_statement_call_bad() {
  infinite_loop_bad();
  nop();
}

void FN_unreachable_statement_exit_bad() {
  exit(2);
  nop();
}

void FN_unreachable_statement_return_bad() {
  return;
  nop();
}

void FN_unreachable_statement_break_bad() {
  while (__infer_nondet_int()) {
    if (__infer_nondet_int()) {
      break;
      nop();
    }
  }
}

void FN_unreachable_statement_continue_bad() {
  while (__infer_nondet_int()) {
    continue;
    nop();
  }
}
