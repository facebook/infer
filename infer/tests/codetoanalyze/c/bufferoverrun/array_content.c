/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdio.h>

int check_sorted_arr_good(int a[], int length) {
  for (int i = 1; i < length; i++) {
    if (a[i] < a[i - 1]) { // should not report CONDITION_ALWAYS_FALSE
      return 0;
    }
  }
  return 1;
}

int check_sorted_arr10_good(int a[10], int length) {
  for (int i = 1; i < length; i++) {
    if (a[i] < a[i - 1]) { // should not report CONDITION_ALWAYS_FALSE
      return 0;
    }
  }
  return 1;
}

int check_sorted_ptr_good(int* a, int length) {
  for (int i = 1; i < length; i++) {
    if (a[i] < a[i - 1]) { // should not report CONDITION_ALWAYS_FALSE
      return 0;
    }
  }
  return 1;
}

int array_min_index_from_one(int* a, int length) {
  int index_min = 1;
  for (int i = 2; i < length; i++) {
    if (a[i] < a[index_min]) { // should not report CONDITION_ALWAYS_FALSE
      index_min = i;
    } else {
      index_min = i;
    }
  }
  return index_min;
}

/*
  We need either a narrowing or a relational domain to prove that
  index_min < length
*/
void call_array_min_index_from_one_good() {
  int a[2];
  a[array_min_index_from_one(a, 2) - 1] = 0;
}

void weak_update_Good_FP() {
  int a[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 15};
  a[a[3]] = 3;
}

void weak_update_Bad() {
  int a[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
  if (a[0] == 0) {
    a[15] = 1;
  }
}

void strong_update_malloc_Good() {
  int a[10];
  int* x = (int*)malloc(sizeof(int));
  *x = 10;
  *x = 0;
  a[*x] = 0;
}

void strong_update_malloc_Bad() {
  int a[10];
  int* x = (int*)malloc(sizeof(int));
  *x = 0;
  *x = 10;
  a[*x] = 0;
}

void weak_update_malloc_Good_FP() {
  int a[10];
  int* x = (int*)malloc(sizeof(int) * 2);
  x[0] = 0;
  x[1] = 10;
  a[x[0]] = 0;
}

void weak_update_malloc_Bad() {
  int a[10];
  int* x = (int*)malloc(sizeof(int) * 2);
  x[0] = 0;
  x[1] = 10;
  a[x[1]] = 0;
}

void weak_update_malloc2_Bad_FN() {
  int a[10];
  int* x = (int*)malloc(sizeof(int) * 2);
  x[0] = 0;
  a[x[1]] = 0;
}

void literal_string_Good() {
  int a[1];
  char* s = "hello";
  for (int i = 0; i < 5; i++) {
    if (s[i] > 'o') {
      a[s[i]] = 0;
    }
  }
}

void literal_string_bad() {
  int a[1];
  char* s = "hello";
  for (int i = 0; i < 5; i++) {
    if (s[i] > 'n') {
      a[s[i]] = 0;
    }
  }
}

void literal_string2_Good() {
  int a[1];
  char s[] = "hello";
  for (int i = 0; i < 5; i++) {
    if (s[i] > 'o') {
      a[s[i]] = 0;
    }
  }
}

void literal_string2_bad() {
  int a[1];
  char s[] = "hello";
  for (int i = 0; i < 5; i++) {
    if (s[i] > 'n') {
      a[s[i]] = 0;
    }
  }
}

void literal_string_parameter(char* s) {
  int a[112]; // 'o' is 111, 'p' is 112
  a[s[0]] = 0;
}

void call_literal_string_parameter1_Good() {
  char* s = "hello";
  literal_string_parameter(s);
}

void call_literal_string_parameter1_Bad() {
  char* s = "hellp";
  literal_string_parameter(s);
}

void call_literal_string_parameter2_Good() {
  literal_string_parameter("hello");
}

void call_literal_string_parameter2_Bad() { literal_string_parameter("hellp"); }

void strlen_constant_Good() {
  int a[10];
  char* s = "hello";
  a[strlen(s)] = 0;
}

void strlen_constant_Bad() {
  int a[5];
  char* s = "hello";
  a[strlen(s)] = 0;
}

void strlen_malloc_Good() {
  int a[10];
  char* s = (char*)malloc(sizeof(char) * 6);
  s[0] = 'h';
  s[1] = 'e';
  s[2] = 'l';
  s[3] = 'l';
  s[4] = 'o';
  s[5] = '\0';
  a[strlen(s)] = 0;
}

void strlen_malloc_Bad() {
  int a[5];
  char* s = (char*)malloc(sizeof(char) * 6);
  s[0] = 'h';
  s[1] = 'e';
  s[2] = 'l';
  s[3] = 'l';
  s[4] = 'o';
  s[5] = '\0';
  a[strlen(s)] = 0;
}

void strlen_malloc_2_Good_FP() {
  int a[5];
  char* s = (char*)malloc(sizeof(char) * 6);
  s[0] = 'h';
  s[1] = 'e';
  s[2] = '\0';
  s[3] = 'l';
  s[4] = 'o';
  s[5] = '\0';
  a[strlen(s)] = 0;
}

void fgets_null_check_Good() {
  char line[100];
  while (fgets(line, 100, stdin)) {
    line[strlen(line) - 1] = 0;
  }
}

void fgets_null_check_Bad() {
  char line[100];
  while (fgets(line, 100, stdin)) {
    line[strlen(line) - 2] = 0;
  }
}

static char file_buf[] = "foo";

void fgets_may_not_change_str_Good_FP() {
  FILE* stream = fmemopen(file_buf, strlen(file_buf), "r");
  fgetc(stream);
  fgetc(stream);
  fgetc(stream);
  char buf[6] = "aaaaa";
  // end-of-file is encountered, thus [buf] should not change.
  fgets(buf, 6, stream);
  int a[5];
  a[9 - strlen(buf)] = 0;
}

void fgets_may_not_change_str_Bad() {
  FILE* stream = fmemopen(file_buf, strlen(file_buf), "r");
  fgetc(stream);
  fgetc(stream);
  fgetc(stream);
  char buf[6] = "aaaaa";
  // end-of-file is encountered, thus [buf] should not change.
  fgets(buf, 6, stream);
  int a[5];
  a[strlen(buf)] = 0;
}
