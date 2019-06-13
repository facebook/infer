/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
