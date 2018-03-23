/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

typedef double vector4 __attribute__((ext_vector_type(4)));

int main() {
  vector4 x = {1, 2, 3, 4};
  vector4 y = 2 * x;
  return 0;
}
