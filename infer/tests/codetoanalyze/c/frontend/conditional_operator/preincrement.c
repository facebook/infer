/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
