/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "global.h"

void use_global_ok() {
  set_global_42();
  if (g != 42) {
    int* p = nullptr;
    *p = 42;
  }
}
