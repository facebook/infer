/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
extern int nondet_int();
extern char* nondet_string();

void loop() {
  int i = 0;
  char* s = nondet_string();

  while (nondet_int()) {
    s[i++] = "0123456789ABCDEF"[nondet_int() % 16];
  }
}
