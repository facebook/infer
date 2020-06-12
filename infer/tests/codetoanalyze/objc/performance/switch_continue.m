/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
int test_switch_FN() {
  int value = 0;
  // infinite loop
  while (value < 100) {
    switch (value) {
      // code before the first case statement gets skipped but can be used to
      // declare variables
      int x = 1;
      x = value + 1;
      case 0:
        break;
      case 1:
        continue;
      case 2:
      default:
        continue;
    }
    value++;
  }
  return 0;
}

int unroll_loop(int n) {
  int ret = 0;
  int loop = n + 3 / 4;
  switch (n % 8) {
    case 0:
      do {
        ret++;
        case 3:
          ret++;
          if (1) {
            case 2:
              ret++;
          }
        case 1:
          ret++;
      } while (--loop > 0);
  }
  return ret;
}
