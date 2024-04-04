/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "library.h"

struct my_pair twice(int n) { return make_pair(n, n); }

int main() {
  struct my_pair p = twice(42);
  return p.x + p.y;
}
