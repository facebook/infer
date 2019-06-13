/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

char* c() { return malloc(12); }
int main() {
  char* s = c();
  return s ? *s : 1;
}
