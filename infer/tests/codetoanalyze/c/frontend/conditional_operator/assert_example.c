/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <assert.h>

struct MyPoint {
  int x;
};

int test(struct MyPoint* activeq) {
  assert(activeq != 0);
  return activeq->x;
}
