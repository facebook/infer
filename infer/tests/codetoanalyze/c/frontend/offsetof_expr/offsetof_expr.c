/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <stddef.h>

struct address {
  char v1[2];
  char v2[5];
  int v3;
};

int test_offsetof_expr() {
  int i = offsetof(struct address, v2);
  if (i == 9) {
    return 9 / 0;
  } else {
    return 4 / 0;
  }
}
