/*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdio.h>
#include <stdlib.h>

void exit_bo_good_unreachable_bad() {
  int arr[1];
  exit(1);
  // unreachable so no buffer overrun
  arr[42] = 42;
}

void fgetc_m1_bad(FILE* f) {
  int arr[10000];
  int c = fgetc(f);
  arr[c] = 42;
}

void fgetc_255_bad(FILE* f) {
  int arr[255];
  int c = fgetc(f);
  if (c >= 0) {
    arr[c] = 42;
  }
}

void fgetc_256_good(FILE* f) {
  int arr[256];
  int c = fgetc(f);
  if (c >= 0) {
    arr[c] = 42;
  }
}

void fgetc_256_bad(FILE* f) {
  int arr[256];
  int c = fgetc(f);
  arr[c + 1] = 42;
}

void fgetc_257_good(FILE* f) {
  int arr[257];
  int c = fgetc(f);
  arr[c + 1] = 42;
}
