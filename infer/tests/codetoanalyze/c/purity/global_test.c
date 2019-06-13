/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int global;

static int s;

void static_incr_bad() { s += 1; }

void global_write_bad(int x, int y) { global += x + y; }

void call_impure_bad(int size) {
  for (int i = 0; i < size; i++) {
    global_write_bad(i, size);
  }
}

int local_write_ok(int x, int y) {
  int k = x + y;
  k++;
  return k;
}

// calls foo which modifies global var
void call_set_bad() { static_incr_bad(); }
