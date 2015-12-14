/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
