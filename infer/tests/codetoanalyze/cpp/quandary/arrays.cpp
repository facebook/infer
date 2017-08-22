/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <array>
#include <string>

extern int __infer_taint_source();
extern void skip(int i, int j);

namespace arrays {

void array_sink1_bad(int arr[]) {
  int source = __infer_taint_source();
  arr[source] = 2;
}

int array_sink2_bad(int arr[]) {
  int source = __infer_taint_source();
  return arr[source];
}

int array_sink3_bad(int arr[]) { return arr[1 + __infer_taint_source()]; }

void array_sink4_bad(int arr[]) {
  int source = __infer_taint_source();
  skip(1, arr[source]);
}

void std_array_sink_bad(std::array<int, 2> arr) {
  int source = __infer_taint_source();
  arr[source] = 2;
}

void std_string_sink_bad(std::string str) {
  int source = __infer_taint_source();
  str[source] = 'a';
}

int stack_smash_bad() {
  int source = __infer_taint_source();
  int arr[source];
  return arr[0]; // could read from anywhere in the stack
}

void strcpy_bad(char* str) {
  char* source = getenv("some_var");
  strcpy(str, source);
}

void memcpy_bad(void* data1, void* data2) {
  int source = __infer_taint_source();
  memcpy(data1, data2, source);
}

void wmemcpy_bad(wchar_t* data1, wchar_t* data2) {
  int source = __infer_taint_source();
  wmemcpy(data1, data2, source);
}

void memmove_bad(void* data1, void* data2) {
  int source = __infer_taint_source();
  memmove(data1, data2, source);
}

void wmemmove_bad(wchar_t* data1, wchar_t* data2) {
  int source = __infer_taint_source();
  wmemmove(data1, data2, source);
}

void memset_bad(char* str) {
  int source = __infer_taint_source();
  memset(str, '0', source);
}

void strncpy_bad(char* str1, char* str2) {
  int source = __infer_taint_source();
  strncpy(str1, str2, source);
}
void copies_ok(char* str) {
  char* source = getenv("some_var");
  int len = std::min(strlen(str), strlen(source));
  memcpy(str, source, len);
  memmove(str, source, len);
  memset(str, '0', len);
  strncpy(str, source, len);
}

// these examples used to crash the HIL conversion
char index_of_literal_ok1() { return "foo"[1]; }

char index_of_literal_ok2() { return "foo"[7]; }

char index_of_literal_ok3(int i) { return "foo"[i]; }
}
