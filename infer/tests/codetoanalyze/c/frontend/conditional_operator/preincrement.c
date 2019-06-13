/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct s {
  int x;
};

void preincrement(struct s* p) {
  p->x += 1;
  (1 ? p : p)->x += 1;
  p->x += 1 ? 3 : 7;
  (1 ? p : p)->x += 1 ? 3 : 7;
}
