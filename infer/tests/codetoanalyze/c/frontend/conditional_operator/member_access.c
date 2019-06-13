/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct s {
  int field;
};

struct s* ret_ptr(int);

void ife_then_access_field(struct s* p, struct s* q) {
  int z = (1 ? p : q)->field;
}

void call_ife_then_access_field() { int z = (ret_ptr(1 ? 2 : 3))->field; }

void access_field_in_ife_branch() { int z = 1 ? (ret_ptr(4))->field : 0; }
