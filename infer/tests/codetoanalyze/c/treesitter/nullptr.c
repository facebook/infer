/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void null_deref_Bad() {
  int* p = 0;
  *p = 42;
}

void null_deref_guarded_Ok(int* p) {
  if (p != 0) {
    *p = 42;
  }
}

void short_circuit_guard_Ok(int* p) {
  if (p != 0 && *p > 0) {
    *p = 1;
  }
}

struct Node {
  int value;
};

void arrow_null_deref_Bad() {
  struct Node* n = 0;
  int v = n->value;
}

void arrow_store_Ok(struct Node* n) {
  n->value = 42;
}
