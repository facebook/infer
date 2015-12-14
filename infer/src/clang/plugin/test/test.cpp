/*
 * Copyright (c) 2004 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

int main() { return 0; }

void f() {
  for (int i = 0; i < 10; i++) {
    ;
  }
}

void g() {
  for (int i = 1; i < 10; i++) {
    ;
  }
}
