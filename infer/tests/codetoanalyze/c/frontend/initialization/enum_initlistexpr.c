/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
union U {
  int f1;
  int f2[3];
};

void union_initialize_FIXME() {
  union U set_f1 = {.f1 = 2};
  union U set_f2 = {.f2 = {1, 2, 3}};
  union U set_f1_implicit = {1};
}
