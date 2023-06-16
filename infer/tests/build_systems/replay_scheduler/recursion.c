/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void goo(int x);
void unknown();

void foo() {
  goo(1);
  unknown();
}

void moo() {}

void goo(int x) {
  if (x > 0) { foo(); };
  moo();
}
