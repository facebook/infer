/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

enum Foo { A, B, C = 10, D, E = 1, F, G = F + C };

int other_enum_main() {
  enum Foo foo_a = A;
  enum Foo foo_b = B;
  enum Foo foo_c = C;
  enum Foo foo_d = D;
  enum Foo foo_e = E;
  enum Foo foo_f = F;
  enum Foo foo_g = G;
}

void enum_values_ok() {
  enum Foo foo_g = G;
  enum Foo foo_a = A;
  if (foo_g != 12 || foo_a != 0) {
    int* p = NULL;
    *p = 42;
  }
}

void enum_values_bad() {
  enum Foo foo_g = G;
  enum Foo foo_a = A;
  if (foo_g == 12 && foo_a == 0) {
    int* p = NULL;
    *p = 42;
  }
}
