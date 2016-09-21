/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
