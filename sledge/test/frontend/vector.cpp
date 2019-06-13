/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef double vector4 __attribute__((ext_vector_type(4)));

int main() {
  vector4 x = {1, 2, 3, 4};
  vector4 y = 2 * x;
  return 0;
}
