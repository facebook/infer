/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef struct _ {
  int* f;
  int* g;
} S;

void main() {
  S x;
  int** p;
  int y;

  /* safe */
  x.f = &y;
  p = &x.f;
  p++;
  /* unsafe */
  *p = 0;
  *x.g = 0;
}
