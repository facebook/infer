/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void ifthenderef(bool b, int* x) {
  if (b) {
    *x = 42;
  }
}

void ifnotthenderef(bool b, int* x) {
  if (!b) {
    *x = 42;
  }
}

void call_ifthenderef_false_null_ok() { ifthenderef(false, nullptr); }

void call_ifthenderef_true_null_bad() { ifthenderef(true, nullptr); }

void call_ifnotthenderef_true_null_ok() { ifnotthenderef(true, nullptr); }

void call_ifnotthenderef_false_null_bad() { ifnotthenderef(false, nullptr); }

// should be FN given the current "all allocated addresses are assumed
// disjoint unless specified otherwise" but we detect the bug because
// we don't propagate pure equalities that we discover to the heap part
void alias_null_deref_bad(int* x, int* y) {
  *x = 32;
  *y = 52;
  if (x == y) {
    // here we have x|-> * x |-> which should be a contradiction
    int* p = nullptr;
    *p = 42;
  }
}

void diverge_if_alias_ok(int* x, int* y) {
  if (x == y) {
    for (;;)
      ;
  }
}

void FP_diverge_before_null_deref_ok(int* x) {
  diverge_if_alias_ok(x, x);
  int* p = nullptr;
  *p = 42;
}
