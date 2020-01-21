/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
static constexpr int ConstantGlobal[] = {5};

void access_constant_global_Bad() {
  int a[5];
  a[ConstantGlobal[0]] = 3;
}

static int StaticGlobal[][3] = {
    {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}};

void access_static_global1_Bad_FN() { int* p = StaticGlobal[10]; }

void access_static_global2_Bad() { int a = StaticGlobal[0][10]; }
