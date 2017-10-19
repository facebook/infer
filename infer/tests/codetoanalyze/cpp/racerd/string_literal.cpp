/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
