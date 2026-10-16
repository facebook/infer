/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace if_constexpr {

void log_int(int);

constexpr bool kDebug = false;

void read_in_discarded_then_branch_ok(int* p) {
  int v = *p;
  if constexpr (kDebug) {
    log_int(v);
  }
}

void read_in_discarded_else_branch_ok(int* p) {
  int v = *p;
  if constexpr (!kDebug) {
    log_int(0);
  } else {
    log_int(v);
  }
}

void init_statement_read_in_discarded_branch_ok(int* p) {
  if constexpr (int v = *p; kDebug) {
    log_int(v);
  }
}

void loop_variable_read_in_discarded_branch_ok(int* p, int n) {
  for (int i = 0, v = *p; i < n; i++) {
    if constexpr (kDebug) {
      log_int(v);
    }
  }
}

void captured_in_discarded_branch_ok(int* p) {
  int v = *p;
  if constexpr (kDebug) {
    [&]() { log_int(v); }();
  }
}

void not_read_in_discarded_branch_bad(int* p) {
  int v = *p;
  int w = *p;
  if constexpr (kDebug) {
    log_int(v);
  }
}

// variables read in a discarded branch are ignored in the whole function
void FN_dead_store_after_discarded_branch_bad(int* p) {
  int v = *p;
  if constexpr (kDebug) {
    log_int(v);
  }
  v = 1;
}

// clang replaces the discarded branch of a template instantiation by an empty
// statement, so the read is lost
template <bool Debug>
void FP_read_in_discarded_branch_of_instantiation_ok(int* p) {
  int v = *p;
  if constexpr (Debug) {
    log_int(v);
  }
}

void call_instantiation(int* p) {
  FP_read_in_discarded_branch_of_instantiation_ok<false>(p);
}

} // namespace if_constexpr
