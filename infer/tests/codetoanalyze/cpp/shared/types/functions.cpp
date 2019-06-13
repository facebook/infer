/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int fun_default(int a = 3, int b = 5) { return a + b; }

int fun_default_decl(int a, int b = 5);
// note that b is default param, but function was declared earlier
int fun_default_decl(int a, int b) { return a + b; }

int fun_ignore_param(int a, int, int) { return a; }

void test() {
  fun_default(1, 2);
  fun_default(1);
  fun_default();

  fun_default_decl(6);
  fun_default_decl(6, 6);
}

void test2() { fun_ignore_param(1, 1, 1); }
