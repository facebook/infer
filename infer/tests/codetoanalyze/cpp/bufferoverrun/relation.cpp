/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <cstdlib>

void minus_params_Ok(int x, int y) {
  int a[5];
  if (0 <= x - y && x - y < 5) {
    a[x - y] = 0;
  }
}

void call1_minus_params_Ok() { minus_params_Ok(5, 2); }

void call2_minus_params_Ok() { minus_params_Ok(10, 2); }

void plus_params(int x, int y) {
  int a[5];
  if (x + y > 0) {
    a[x + y - 1] = 0;
  }
}

void call1_plus_params_Ok() { plus_params(1, 2); }

void call2_plus_params_Bad() { plus_params(10, 2); }

void call3_plus_params_Ok() { plus_params(0, 0); }

void plus_params2(int x, int y) {
  int a[5];
  if (-x < y) {
    a[x + y - 1] = 0;
  }
}

void call1_plus_params2_Ok() { plus_params2(1, 2); }

void call2_plus_params2_Bad() { plus_params2(10, 2); }

void call3_plus_params2_Ok() { plus_params2(0, 0); }

void loop(char* arr, int len) {
  while (len > 0) {
    arr[0] = 0;
    arr += 1;
    len -= 1;
  }
}

void FP_call1_loop_Ok() {
  char arr[5];
  loop(arr, 5);
}

void call2_loop_Bad() {
  char arr[5];
  loop(arr, 10);
}

void FP_loop2_Ok() {
  int len = 12;
  char* arr = (char*)malloc(len * sizeof(char));
  while (len >= 4) {
    arr += 4;
    len -= 4;
  }
  switch (len) {
    case 3:
      arr[2] = 0;
    case 2:
      arr[1] = 0;
    case 1:
      arr[0] = 0;
  };
}

void loop_with_type_casting(void* data, int len) {
  char* arr = (char*)data;
  while (len >= 4) {
    int k = *(int*)arr;
    arr += 4;
    len -= 4;
  }
  switch (len) {
    case 3:
      arr[2] = 0;
    case 2:
      arr[1] = 0;
    case 1:
      arr[0] = 0;
  };
}

typedef struct s {
  int a;
  int b;
} s_t;

void FP_call_loop_with_type_casting_Ok() {
  s_t* c = (s_t*)malloc(sizeof(s_t));
  loop_with_type_casting(c, 8);
}

size_t id(size_t s) {
  if (s == 0) {
    return 0;
  } else {
    return s;
  }
}

void FP_call_id_Ok() {
  size_t s1 = 5;
  size_t s2 = id(1 + s1);
  char* arr = (char*)malloc(s2 * sizeof(char));
  arr[s1] = 0;
}
