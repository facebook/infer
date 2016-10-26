/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include "B.h"

int field_superclass_main() {
  B* b = [B alloc];
  b->x = 5;
  b->a = b; // create cycle --> leak
  return 0;
}
