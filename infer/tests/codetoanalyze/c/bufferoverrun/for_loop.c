/*
 * Copyright (c) 2016-present, Programming Research Laboratory (ROPAS)
 *                             Seoul National University, Korea
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

char* safealloc(int n) {
  char* x;
  if (n > 0)
    x = malloc(n);
  else
    x = malloc(10);

  if (!x)
    return x;
  else
    exit(1);
}

void for_loop() {
  char* a;
  int i;

  a = safealloc(10);
  for (i = 0; i < 10; i++) {
    a[i] = 'a'; /* SAFE */
  }
  a = safealloc(5);
  for (i = 0; i < 10; i++) {
    a[i] = 'a'; /* BUG */
  }
}

void no_op() { int k = 0; }

int two_loops(int m) {
  for (int i = 0; i < m; i++) {
    no_op();
  }
  for (int j = 0; j < m; j++) {
    no_op();
  }
  return m;
}

void call_two_loops_Good() {
  int a[10];
  int m = 5;
  a[two_loops(m)] = 1;
}

void call_two_loops_Bad() {
  int a[10];
  int m = 15;
  a[two_loops(m)] = 1;
}

struct payload {
  int count;
  int payload[];
};

#define COUNT 10

// memleak but no array out of bounds error
void malloc_sizeof_value_leak_good() {
  struct payload* x;
  x = malloc(sizeof(*x) + COUNT * sizeof(x->payload[0]));
  if (x == NULL) {
    return 1;
  }
  x->count = COUNT;
  for (int i = 0; i < COUNT; i++) {
    x->payload[i] = i;
  }
  /* missing free(x) */
}

void initialize_arr(int* arr, int count) {
  for (int i = 0; i < count; ++i) {
    arr[i] = 0;
  }
}

void call_initialize_arr_Good() {
  int arr[10];
  initialize_arr(arr, 5);
}

void call_initialize_arr_Bad() {
  int arr[10];
  initialize_arr(arr, 20);
}

void threshold_by_comparison_1_Good() {
  int arr[100];
  for (int i = 0; i != 100; i++) {
    arr[i] = 0;
  }
}

void threshold_by_comparison_1_Bad() {
  int arr[50];
  for (int i = 0; i != 100; i++) {
    arr[i] = 0;
  }
}

void threshold_by_comparison_2_Good() {
  int arr[100];
  int j = 0;
  while (1) { // widening threshold for j should be 99, not 100
    j++;
    if (j == 100) {
      j = 0;
    }
    arr[j] = 0;
  }
}

void threshold_by_comparison_2_Bad() {
  int arr[50];
  int j = 0;
  while (1) { // widening threshold for j should be 99, not 100
    j++;
    if (j == 100) {
      j = 0;
    }
    arr[j] = 0;
  }
}

void infinite_for_loop_Good() {
  int arr[5];
  int x = 0;
  for (x;;) {
  }
  arr[10] = 0;
}
