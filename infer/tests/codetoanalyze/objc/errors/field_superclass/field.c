/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "B.h"

int field_superclass_main() {
  B* b = [B alloc];
  b->x = 5;
  b->a = b; // create cycle --> leak
  return 0;
}
