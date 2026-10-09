/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>
#include "struct_with_enum.h"

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

void block_scope_enum_values_ok() {
  enum { kZero, kTwo = 2, kThree };
  if (kZero != 0 || kTwo != 2 || kThree != 3) {
    int* p = NULL;
    *p = 42;
  }
}

void block_scope_enum_values_bad() {
  enum { kZero, kTwo = 2, kThree };
  if (kZero == 0 && kTwo == 2 && kThree == 3) {
    int* p = NULL;
    *p = 42;
  }
}

void block_scope_enum_and_var_bad() {
  enum Bar { kFive = 5, kSix } six = kSix;
  if (six == 6) {
    int* p = NULL;
    *p = 42;
  }
}

void block_scope_typedef_enum_bad() {
  typedef enum { kSeven = 7 } Seven;
  Seven seven = kSeven;
  if (seven == 7) {
    int* p = NULL;
    *p = 42;
  }
}

void enum_in_block_scope_struct_bad() {
  struct Local {
    enum { kEight = 8 } k;
  } local;
  if (kEight == 8) {
    int* p = NULL;
    *p = 42;
  }
}

// records declared in other headers are not translated, so the enum inside is
// only known through its constant
void enum_in_header_struct_bad() {
  if (kNine == 9) {
    int* p = NULL;
    *p = 42;
  }
}

void enum_declared_after_deref_bad() {
  int* p = NULL;
  *p = 42;
  enum { kTen = 10 };
}
