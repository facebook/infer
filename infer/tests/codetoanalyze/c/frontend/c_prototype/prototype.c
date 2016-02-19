/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

/*
 A simple example of a function prototype allowing the function
 to be used before it is defined.
 */

int sum(int, int);

int main(void) {
  int total;

  total = sum(2, 3);

  return 0;
}

int sum(int a, int b) { return a + b; }
