/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void some_f(int, int, int);

void fun_ifthenelse1() { (1 ? some_f : some_f)(1, 2, 3); }

void fun_ifthenelse2() {
  (1 ? some_f : some_f)(0 ? 1 : 1, 0 ? 2 : 2, 0 ? 3 : 3);
}

void fun_ifthenelse3() { some_f(0 ? 1 : 1, 0 ? 2 : 2, 0 ? 3 : 3); }

void fun_ifthenelse4() { (1 ? some_f : some_f)(0 ? 1 : 1, 2, 0 ? 3 : 3); }
