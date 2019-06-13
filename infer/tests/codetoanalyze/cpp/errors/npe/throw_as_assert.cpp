/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
