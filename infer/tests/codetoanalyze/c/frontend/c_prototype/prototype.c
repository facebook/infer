/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
