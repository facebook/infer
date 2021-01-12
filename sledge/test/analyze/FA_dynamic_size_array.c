/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void main() {
  size_t n = __llair_choice();
  n = (n < 0 ? -n : n) + 1;
  int* a = __llair_alloc(n * sizeof(int));

  for (int i = 0; i < n; i++) {
    a[i] = i;
  }

  free(a);

  return;
}
