/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define my_macro __extension__({ int __s1 = 42; })

void goo(int);

void foo(int b) {
  int x = 42;

  if (b) {
    goo(x);
    my_macro;
  }
}
