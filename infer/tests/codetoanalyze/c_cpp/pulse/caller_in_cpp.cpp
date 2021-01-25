/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>
#include "callee_in_c.h"

void call_set_field_three_Ok(int* p) {
  struct s x;
  x.a = 5;
  set_field_three(&x);
  if (x.a == 5) {
    free(p);
    *p = 3;
  }
}
