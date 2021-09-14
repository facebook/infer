/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
void exit_unreachable() {
  exit(0); // modeled as unreachable
}

// constraint solver resolves all nodes to unreachable cost
void compute_exit_unreachable() {
  int k = 0;
  exit(0);
}

void linear(int p) {
  for (int i = 0; i < p; i++) {
  }
}

void call_exit_unreachable(int p) {
  linear(p);
  exit(0);
}

// constraint solver doesn't behave consistently and gets confused
// when resolving constraints and gets linear cost
void inline_exit_unreachable_FP(int p) {
  for (int i = 0; i < p; i++) {
  }
  exit(0);
}

void call_unreachable() { exit_unreachable(); }
