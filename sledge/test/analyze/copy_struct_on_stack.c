/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef struct _ {
  int i;
  int j;
  char c;
} S;

void main() {
  S x = {1, 2, '3'};
  S y = {4, 5, '6'};

  x = y;
}
