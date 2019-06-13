/*
 * Copyright (c) 2016-present, Programming Research Laboratory (ROPAS)
 *                             Seoul National University, Korea
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void nested_loop() {
  int i, j;
  char a[10];

  for (i = 0; i < 10; i++) {
    a[i] = 'a'; /* SAFE */
    for (j = 0; j <= 10; j++) {
      a[j] = 'a'; /* BUG */
    }
  }
}

void nested_loop2_Ok() {
  double arr[10];

  int i, j;
  for (i = 0; i < 10; i++) {
    for (j = 5; j < 15; j++) {
      arr[i] = 0.0;
      arr[j - 5] = 0.0;
    }
  }
}

void nested_loop3_Bad() {
  double arr[10];

  int i, j;
  for (i = 0; i <= 10; i++) {
    for (j = 5; j < 15; j++) {
      arr[i] = 0.0;
    }
  }
}

void nested_loop4_Bad() {
  double arr[10];

  int i, j;
  for (i = 0; i < 10; i++) {
    for (j = 5; j <= 15; j++) {
      arr[j - 5] = 0.0;
    }
  }
}

void nested_loop_narrowing_Good() {
  int i = 0;
  int a[10];

  while (1) {
    while (1) {
      a[i] = 0;
      for (i = 0; i < 5; i++) {
      }
    }
  }
}

void nested_loop_narrowing_Bad() {
  int i = 0;
  int a[10];

  while (1) {
    while (1) {
      a[i] = 0;
      for (i = 0; i < 10; i++) {
      }
    }
  }
}
