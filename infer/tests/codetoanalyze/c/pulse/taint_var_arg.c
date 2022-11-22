/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdio.h>

int int_source();

float float_source();

void printf_source_bad1() { printf("Integers: %i %u \n", -3456, int_source()); }

void printf_source_bad2() {
  printf("Some different radices: %d %x %o %#x %#o \n",
         100,
         int_source(),
         100,
         100,
         100);
}

void printf_source_bad3() {
  printf("floats: %4.2f %+.0e %E \n", 3.14159, 3.14159, float_source());
}

void printf_source_bad4() {
  printf("Preceding with zeros: %010d \n", int_source());
}
