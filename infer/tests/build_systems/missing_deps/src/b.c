/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int b_get(int* ptr) { return *ptr; }

void b() {
  int* ptr = 0;

  int x = b_get(ptr);
}
