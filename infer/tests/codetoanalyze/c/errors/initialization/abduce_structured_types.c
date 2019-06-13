/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef struct {
  int f1;
} a_struct;

typedef struct {
  int f1;
  a_struct array[10];
} b_struct;

void init_static() { static const b_struct x[] = {{1}}; }

int array_of_struct(b_struct x[]) {
  int j = x[0].array[5].f1;
  int i = x[0].f1;
  b_struct y = x[0];
}

int pointer_of_struct(b_struct* x) {
  int i = (*x).f1;
  b_struct y = *x;
  int j = y.f1;
}

int array_of_array_of_struct(b_struct x[][100]) {
  int i = x[32][52].f1;
  b_struct* y = x[32];
  b_struct z = x[32][52];
  int j = z.f1;
}

int pointer_of_array_of_struct(b_struct* x[], int n) {
  int i = (*x)[n].f1;
  b_struct y = (*x)[n];
  int j = y.f1;
}
