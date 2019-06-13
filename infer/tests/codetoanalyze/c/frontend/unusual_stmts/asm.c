/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int test() {
  int x;
  int y;
  int z;
  int h;
  asm("cpuid " : "=a"(x), "=b"(y), "=c"(z), "=d"(h) : "0"(0));
  return 0;
}

int main() {
  int src = 1;
  int dst;

  asm("mov %1, %0\n\t"
      "add $1, %0"
      : "=r"(dst)
      : "r"(src));
  return 0;
}
