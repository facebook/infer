/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <stdlib.h>
#include <cstddef>

struct foo {
  void check_condition(bool cond) {
    if (!cond) {
      throw 1;
    }
  }

  void test_that_throw_prunes_condition() {
    int* p = (int*)malloc(sizeof(int));
    check_condition(p != nullptr);
    *p = 42;
    free(p);
  }
};
