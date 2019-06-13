/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
int global;

void compare_global_variable_bad() {
  char arr[10];
  if (global < 10)
    arr[10] = 1;
}

const int global_const_zero = 0;

enum { global_const = global_const_zero };

void compare_global_const_enum_Bad() {
  char arr[10];
  if (global_const < 10)
    arr[10] = 1;
}

void compare_global_const_enum_Good_FP() {
  char arr[10];
  if (global_const > 10)
    arr[10] = 1;
}

const int global_const_ten = 10;

void use_global_const_ten_Good() {
  char arr[20];
  arr[global_const_ten] = 0;
}

void use_global_const_ten_Bad() {
  char arr[5];
  arr[global_const_ten] = 0;
}

static const char global_arr[] = {1, 0, 1};

static void copyfilter_Good_FP(const char* s, const char* z, int b) {
  int i;
  int n = strlen(s);
  for (i = 0; z[i]; i++) { // We need to infer that z[i] means i < strlen(z)
    if (global_arr[z[i]] || // We need a weak update here
        (z[i] == s[0] && (n == 1 || memcmp(z, s, n) == 0))) {
      i = 0;
      break;
    }
  }
}

static const char* global_string_array[] = {"a", "b", "c", "d", "e", "f"};

#define ISSUE949_SIZE 50

int issue949_arr[ISSUE949_SIZE];

void issue949_bad_FN() {
  for (int i = 0; i <= ISSUE949_SIZE; i++) {
    issue949_arr[i] = 1;
  }
}
