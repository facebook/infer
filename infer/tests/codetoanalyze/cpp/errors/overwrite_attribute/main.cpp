/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "lib.h"

int testSetIntValue() {
  int x;
  setIntValue(&x, 0);
  return 1 / x; // div0
}
