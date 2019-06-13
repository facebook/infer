/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

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

int other_enum_test() {
  enum Foo foo_g = G;
  enum Foo foo_a = A;
  if (foo_g == 12)
    return foo_g / foo_a;
  else
    return 0;
}
