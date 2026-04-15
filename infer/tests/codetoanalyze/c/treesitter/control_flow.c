/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int sum(int n) {
  int s = 0;
  for (int i = 0; i < n; i++) {
    s += i;
  }
  return s;
}

int count_digits(int n) {
  int count = 0;
  do {
    count++;
    n = n / 10;
  } while (n > 0);
  return count;
}

int classify(int x) {
  int result = 0;
  switch (x) {
  case 0:
    result = -1;
    break;
  case 1:
    result = 0;
    break;
  default:
    result = 1;
    break;
  }
  return result;
}

void swap(int* a, int* b) {
  int tmp = *a;
  *a = *b;
  *b = tmp;
}
