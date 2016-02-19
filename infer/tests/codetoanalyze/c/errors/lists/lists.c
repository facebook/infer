/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

struct l2 {
  int b;
  struct l2* a;
};

int add2(struct l2* l) {
  int r = 0;
  for (; l; l = l->a) {
    r += l->b;
  }
  return r;
}

/* Divide by zero error shows that we get a spec for add2 */
int main() {
  int res = add2(0);
  return 5 / res;
}
