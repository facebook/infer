/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int main() {
  double x = 1.0;
  double q, r, s, t;
  x = s;
  q = (x = 3);
  x += 7;
  q = (x += 1.0);
  q = (x += (r += (s += t)));
  return 0;
}
