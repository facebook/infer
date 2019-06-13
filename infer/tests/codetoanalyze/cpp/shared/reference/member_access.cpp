/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct X {
  int f;
  int call() { return f; }
};

void access_ref(X& x) {
  int f = x.f;
  int c = x.call();
}

void access_ptr(X* x) {
  int f = x->f;
  int c = x->call();
}
