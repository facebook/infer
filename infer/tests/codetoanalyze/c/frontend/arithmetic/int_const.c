/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

int main() {

  int volatile a;
  int* volatile b;
  float* const c;
  long double d;

  static const int kDuration = 3;

  int large_int = 9223372036854775807;
  int overflow_int = 9223372036854775808;

  return 0;
}
