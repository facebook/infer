/*
 * Copyright (c) 2013 - Facebook.
 * All rights reserved.
 */

void bound_error() {
  int a[7];
  a[7] = 4;
}

void nested_array_ok() {
  int a[3][4][5];
  a[2][3][4] = 0;
}

void bound_error_nested() {
  int a[3][4][5];
  a[4][3][2] = 0;
}
