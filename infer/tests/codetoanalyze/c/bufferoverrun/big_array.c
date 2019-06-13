/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define CHARS_21 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0,
#define CHARS_147 CHARS_21 CHARS_21 CHARS_21 CHARS_21 CHARS_21 CHARS_21 CHARS_21
#define CHARS_882 CHARS_147 CHARS_147 CHARS_147 CHARS_147 CHARS_147 CHARS_147
#define CHARS_5292 CHARS_882 CHARS_882 CHARS_882 CHARS_882 CHARS_882 CHARS_882
#define CHARS_26460 CHARS_5292 CHARS_5292 CHARS_5292 CHARS_5292 CHARS_5292

unsigned char* big_array() {
  const unsigned char bytes[] = {CHARS_26460};
  return bytes;
}

void use_big_array_bad() {
  unsigned char* b = big_array();
  b[999999999] = 'Y';
}
