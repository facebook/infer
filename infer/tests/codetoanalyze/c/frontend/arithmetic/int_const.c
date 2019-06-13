/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
