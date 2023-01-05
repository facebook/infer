/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct my_pair {
  int x;
  int y;
};

struct my_pair make_pair(int x, int y) {
  struct my_pair pair;
  pair.x = x;
  pair.y = y;
  return pair;
}
