/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int foo(int* p) {
  if ((*p = 0)) {
    return 32;
  }
  return 52;
}
