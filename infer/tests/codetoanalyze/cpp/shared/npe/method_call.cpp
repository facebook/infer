/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct X {
  int f;
  int call() { return 1; }
};

int npe_call() {
  X* x = nullptr;
  return x->call();
}

X* getX() { return nullptr; }

void npe_call_after_call() { getX()->call(); }

struct XForward;

struct Y {
  XForward* x;
};

struct XForward {
  int call() { return 0; }
  int f;
};

void call_with_forward_declaration(XForward* x) { x->call(); }

void npe_call_with_forward_declaration() {
  call_with_forward_declaration(nullptr);
}
