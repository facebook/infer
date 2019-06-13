/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
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
