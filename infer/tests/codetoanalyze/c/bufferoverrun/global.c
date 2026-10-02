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

void issue949_bad() {
  for (int i = 0; i <= ISSUE949_SIZE; i++) {
    issue949_arr[i] = 1;
  }
}

void issue949_Good() {
  for (int i = 0; i < ISSUE949_SIZE; i++) {
    issue949_arr[i] = 1;
  }
}

int read_issue949_arr_Bad() { return issue949_arr[ISSUE949_SIZE]; }

void memset_issue949_arr_Bad() {
  memset(issue949_arr, 0, (ISSUE949_SIZE + 1) * sizeof(int));
}

static void write_at(int* a, int i) { a[i] = 0; }

void pass_issue949_arr_Bad() { write_at(issue949_arr, ISSUE949_SIZE); }

void pass_issue949_arr_Good() { write_at(issue949_arr, ISSUE949_SIZE - 1); }

static int global_init_arr[4] = {1, 2, 3, 4};

void store_global_init_arr_loop_Bad() {
  for (int i = 0; i <= 4; i++) {
    global_init_arr[i] = 0;
  }
}

int read_global_init_arr_after_store_Bad() {
  global_init_arr[0] = 0;
  int x = 0;
  for (int i = 0; i <= 4; i++) {
    x = global_init_arr[i];
  }
  return x;
}

void memcpy_global_init_arr_then_store_Bad() {
  int src[4] = {0};
  memcpy(global_init_arr, src, sizeof(global_init_arr));
  global_init_arr[4] = 0;
}

static int global_index_arr[2] = {-1, -1};

static void set_global_index_arr(int v) { global_index_arr[0] = v; }

int read_global_init_arr_after_callee_store_Good() {
  int a[10] = {0};
  set_global_index_arr(5);
  return a[global_index_arr[0]];
}

void read_global_init_arr_under_guard_Bad() {
  int a[2];
  if (global_index_arr[0] != -1) {
    a[2] = 0;
  }
}

static int global_2d_arr[5][3];

void store_global_2d_arr_Bad() {
  for (int i = 0; i < 5; i++) {
    for (int j = 0; j <= 3; j++) {
      global_2d_arr[i][j] = 0;
    }
  }
}

void store_global_2d_arr_Good() {
  for (int i = 0; i < 5; i++) {
    for (int j = 0; j < 3; j++) {
      global_2d_arr[i][j] = 0;
    }
  }
}

static char global_2d_char_arr[4][8];

// the strcpy model adds the copied length to the row index
void FP_strcpy_global_2d_arr_row_Good() {
  strcpy(global_2d_char_arr[3], "1234567");
}

static int global_one_elem_arr[1];

void store_global_one_elem_arr_Bad() { global_one_elem_arr[1] = 0; }

char read_static_local_arr_Bad() {
  static char buf[8] = {0};
  char c = 0;
  for (int i = 0; i <= 8; i++) {
    c = buf[i];
  }
  return c;
}

void store_static_local_arr_Good() {
  static char buf[8];
  for (int i = 0; i < 8; i++) {
    buf[i] = 0;
  }
}

void same_name_static_local_arrs_Good() {
  {
    static int buf[8];
    buf[5] = 0;
  }
  {
    static int buf[2];
    buf[1] = 0;
  }
}

static struct {
  int len;
  int vals[4];
} global_struct = {0, {0}};

int read_global_struct_array_field_Bad() {
  int x = 0;
  for (int i = 0; i <= 4; i++) {
    x = global_struct.vals[i];
  }
  return x;
}

void store_global_struct_array_field_Good() {
  for (int i = 0; i < 4; i++) {
    global_struct.vals[i] = i;
  }
}

// array fields of global structs passed to calls get no size
void FN_pass_global_struct_array_field_Bad() {
  write_at(global_struct.vals, 4);
}

struct item {
  int key;
  int vals[2];
};

static struct item global_items[3];

void store_global_array_of_structs_Bad() {
  for (int i = 0; i <= 3; i++) {
    global_items[i].key = 0;
  }
}

void store_global_array_of_structs_inner_array_Bad() {
  for (int j = 0; j <= 2; j++) {
    global_items[1].vals[j] = 0;
  }
}

void store_global_array_of_structs_Good() {
  for (int i = 0; i < 3; i++) {
    global_items[i].key = i;
    global_items[i].vals[1] = i;
  }
}

extern int global_incomplete_arr[];

void store_global_incomplete_arr_Good(int i) { global_incomplete_arr[i] = 0; }

int* global_ptr;

void store_global_ptr_Bad() {
  int a[4];
  global_ptr = a;
  global_ptr[4] = 0;
}

struct with_pointer {
  int* p;
};

struct with_pointer global_with_pointer;

int read_global_struct_as_pointer_Good() {
  return (*(int**)&global_with_pointer)[2];
}
