/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int check_sorted_arr_good_FP(int a[], int length) {
  for (int i = 1; i < length; i++) {
    if (a[i] < a[i - 1]) { // should not report CONDITION_ALWAYS_FALSE
      return 0;
    }
  }
  return 1;
}

int check_sorted_arr10_good_FP(int a[10], int length) {
  for (int i = 1; i < length; i++) {
    if (a[i] < a[i - 1]) { // should not report CONDITION_ALWAYS_FALSE
      return 0;
    }
  }
  return 1;
}

int check_sorted_ptr_good_FP(int* a, int length) {
  for (int i = 1; i < length; i++) {
    if (a[i] < a[i - 1]) { // should not report CONDITION_ALWAYS_FALSE
      return 0;
    }
  }
  return 1;
}

int array_min_index_from_one_FP(int* a, int length) {
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
  We need a either a narrowing or a relational domain to prove that
  index_min < length
*/
void call_array_min_index_from_one_good() {
  int a[2];
  a[array_min_index_from_one_FP(a, 2) - 1] = 0;
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
