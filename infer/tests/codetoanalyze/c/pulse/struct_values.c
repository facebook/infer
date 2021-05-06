/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

struct inlined {
  int x;
  int y;
};

struct s {
  struct inlined i;
  int f;
  int g;
};

// the analysis should not accidentally expose the mutations to callees
void changes_fields_locally(struct s a) {
  int u = a.i.x;
  a.f = 42;
  a.i.y = 15;
}

void struct_value_in_callee_ok() {
  struct s b = {{11, 22}, 33, 44};
  changes_fields_locally(b);
  if (b.i.x != 11 || b.i.y != 22 || b.f != 33 || b.g != 44) {
    int* p = NULL;
    *p = 42;
  }
}
