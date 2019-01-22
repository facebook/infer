/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stddef.h>

void conditional_buffer_access(int* ptr, unsigned int size) {
  int i;
  if (size < 1) {
  } else if (size < 2) {
    i = *(ptr++);
  } else if (size < 3) {
    i = *(ptr++);
    i = *(ptr++);
  } else if (size < 4) {
    i = *(ptr++);
    i = *(ptr++);
    i = *(ptr++);
  } else if (size < 5) {
    i = *(ptr++);
    i = *(ptr++);
    i = *(ptr++);
    i = *(ptr++);
  }
}

void call_conditional_buffer_access_Good() {
  int a[1];
  conditional_buffer_access(a, 1);
}

void call_conditional_buffer_access_Bad() {
  int a[1];
  conditional_buffer_access(a, 3);
}

void conditional_buffer_access2(unsigned int n) {
  int a[n];
  conditional_buffer_access(a, n);
}

void call_conditional_buffer_access2_1_Good() { conditional_buffer_access2(1); }

void call_conditional_buffer_access2_2_Good() { conditional_buffer_access2(3); }

void conditional_minus(int* ptr, unsigned int size) {
  int i = 0;
  if (ptr != NULL && (i < size - 1)) {
  }
}

void call_conditional_minus_1_Good() { conditional_minus(NULL, 0); }

void call_conditional_minus_2_Good() {
  int a[3];
  conditional_minus(a, 3);
}

void call_conditional_minus_2_Bad() {
  int a[3];
  conditional_minus(a, 0);
}

unsigned int conditional_minus2(int* ptr, unsigned int size) {
  if (ptr != NULL) {
    return (size - 1);
  }
}

void call_conditional_minus2_1_Good() { conditional_minus2(NULL, 0); }

void call_conditional_minus2_2_Good() {
  int a[3];
  conditional_minus2(a, 3);
}

void call_conditional_minus2_2_Bad() {
  int a[3];
  conditional_minus2(a, 0);
}

enum E {
  E_SIZEONE = 0,
  E_SIZETWO = 1,
};

void conditional_buffer_access3(int* ptr, int size) {
  int i;
  switch (ptr[0]) {
    case E_SIZETWO:
      i = ptr[size - 2];
      i = ptr[size - 1];
      break;

    case E_SIZEONE:
      i = ptr[size - 1];
      break;
  }
}

void call_conditional_buffer_access3_1_Good() {
  int a[2];
  a[0] = E_SIZETWO;
  conditional_buffer_access3(a, 2);
}

void call_conditional_buffer_access3_2_Good() {
  int a[1];
  a[0] = E_SIZEONE;
  conditional_buffer_access3(a, 1);
}

void call_conditional_buffer_access3_Bad() {
  int a[1];
  a[0] = E_SIZETWO;
  conditional_buffer_access3(a, 1);
}

void conditional_inequality(int idx) {
  int a[5];
  if (idx == 5) {
  } else {
    a[idx] = 0;
  }
}

void call_conditional_inequality_Good_FP() { conditional_inequality(5); }

void call_conditional_inequality_Bad() { conditional_inequality(6); }
