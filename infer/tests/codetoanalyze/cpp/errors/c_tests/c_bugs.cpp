/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <cstdio>
#include <cstdlib>
#include <cstring>

// basic tests for C bugs in C++ code

void malloc_memory_leak_is_reported() { int* p = (int*)malloc(sizeof(int)); }

void malloc_free_works() {
  int* p = (int*)malloc(sizeof(int));
  free(p);
}

void malloc_fail_gets_reported() {
  int* p = (int*)malloc(sizeof(int));
  *p = 3; // Null dereference
  free(p);
}

void resource_leak_is_reported() { FILE* fd = fopen("hi.txt", "w"); }

void fopen_fclose_works() {
  FILE* fd = fopen("hi.txt", "w");
  if (fd) {
    fclose(fd);
  }
}

void memcpy_spec_is_found() {
  int x;
  // this will cause PRECONDITION_NOT_MET and stop analysis
  memcpy(0, &x, sizeof(int));
  int p = 1 / 0; // infer won't reach it when memcpy spec is found
}

// taken from getc.c e2e test
void crash_getc() {
  FILE* f;
  int i;
  f = fopen("this_file_doesnt_exist", "r");
  i = getc(f);
  fclose(f);
}

// taken from getc.c e2e test
void crash_fgetc() {
  FILE* f;
  int i;
  f = fopen("this_file_doesnt_exist", "r");
  i = fgetc(f);
  fclose(f);
}
