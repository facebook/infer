/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int x;

void main() {
  int n = 4;
  int* A = (int*)__llair_alloc(n * sizeof(int));
  /* safe */
  x = A[0];
  x = A[1];
  x = A[2];
  x = A[3];
}
