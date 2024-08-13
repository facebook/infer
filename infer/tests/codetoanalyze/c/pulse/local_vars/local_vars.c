/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int m1(int z) {
  int y = 0;
  int x = 5;
  if (z < 10) {
    int x = 7;
    if (x == 7)
      return x / y;
  }
  if (x == 6)
    return x / y;
  else
    return 0;
}

int m2(int z) {
  int y = 0;
  int x = 5;
  if (z < 10) {
    int x = 7;
    if (x == 6)
      return x / y;
  }
  if (x == 5)
    return x / y;
  else
    return 0;
}

int mm() {
  int y = 0;
  int x = 0;
  {
    int x = 5;
    if (x == 5)
      return x / y;
  }
  if (x == 0)
    return x / y;
  else
    return 0;
}

int t() {
  int y = 0;
  int x = 1;
  int z = 0;

  for (int x = 0; x < 10; x++) {
    int x = 9;
    if (x == 9)
      return x / y;
    else
      return 0;
  }
  return 0;
}

int address_taken() {
  int** x;
  int* y;
  int i = 7;
  y = &i;
  x = &y;
  // if we don't reason about taken addresses while adding nullify instructions,
  // we'll add
  // `nullify(y)` here and report a false NPE on the next line
  return **x;
}
