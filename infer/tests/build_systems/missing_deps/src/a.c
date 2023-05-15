/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int a_get(int* ptr) { return *ptr; }

void a() {
  int* ptr = 0;

  int x = a_get(ptr);
}
