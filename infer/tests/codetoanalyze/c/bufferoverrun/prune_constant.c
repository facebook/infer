/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
void prune_constant_true_Ok() {
  int a[1];

  if (1) {
    a[0] = 0;
  } else {
    a[1] = 0;
  }
}

void prune_constant_false_Ok() {
  int a[1];

  if (0) {
    a[1] = 0;
  } else {
    a[0] = 0;
  }
}

void prune_constant_value_Ok(int x) {
  int a[1];
  if (-1 < x && x < 1) {
    if (x) {
      a[1] = 0;
    } else {
      a[0] = 0;
    }
  }
}

void prune_constant_not_Bad() {
  int x = 0;
  int a[1];
  if (!x) {
    a[x + 1] = 0;
  }
}

int fromHex(char c) {
  if (c < '0' || (c > '9' && (c < 'a' || c > 'f'))) {
    return -1; // invalid not 0-9a-f hex char
  }
  if (c <= '9') {
    return c - '0';
  }
  return c - 'a' + 10;
}

void call_fromHex_sym_Good(char c) {
  char arr[16];
  int idx = fromHex(c);
  if (idx >= 0) {
    arr[idx] = 'H';
  }
}

void call_fromHex_200_Good() {
  char arr[16];
  int idx = fromHex(200);
  if (idx >= 0) {
    arr[idx] = 'H';
  }
}

void call_fromHex2_sym_Good_FP(char c) {
  char arr[17];
  int idx = fromHex(c);
  arr[idx + 1] = 'H';
}

void call_fromHex2_200_Good_FP() {
  char arr[17];
  int idx = fromHex(200);
  arr[idx + 1] = 'H';
}

void prune_add1(unsigned int x) {
  int a[10];
  if (x + 1 < 11) {
    a[x] = 0;
  }
}

void call_prune_add1_1_Good() { prune_add1(5); }

void call_prune_add1_2_Good() { prune_add1(10); }

void prune_add2(unsigned int x) {
  int a[10];
  if (x + 1 < 12) {
    a[x] = 0;
  }
}

void call_prune_add2_1_Good() { prune_add2(5); }

void call_prune_add2_2_Bad() { prune_add2(10); }

void call_prune_add2_3_Good() { prune_add2(100); }

void prune_sub1(unsigned int x) {
  int a[10];
  if (9 > x - 1) {
    a[x] = 0;
  }
}

void call_prune_sub1_1_Good() { prune_sub1(5); }

void call_prune_sub1_2_Good() { prune_sub1(10); }

void prune_sub2(unsigned int x) {
  int a[10];
  if (10 > x - 1) {
    a[x] = 0;
  }
}

void call_prune_sub2_1_Good() { prune_sub2(5); }

void call_prune_sub2_2_Bad() { prune_sub2(10); }

void call_prune_sub2_3_Good() { prune_sub2(100); }

void null_pruning1_Good(int* p) {
  if (p == 0) {
    if (p != 0) {
      int a[5];
      a[10] = 1;
    }
  }
}

void null_pruning1_Bad(int* p) {
  if (p == 0) {
    if (p == 0) {
      int a[5];
      a[10] = 1;
    }
  }
}

void null_pruning2_Good_FP(int* p) {
  if (p != 0) {
    if (p == 0) {
      int a[5];
      a[10] = 1;
    }
  }
}

void null_pruning2_Bad(int* p) {
  if (p != 0) {
    if (p != 0) {
      int a[5];
      a[10] = 1;
    }
  }
}

int greater_than(unsigned int x, unsigned int y) { return (x > y); }

void call_greater_than_Good() {
  unsigned int idx = 0;
  if (greater_than(idx, 0)) {
    idx = idx - 1;
  }
}

void null_pruning_symbols(unsigned int a, unsigned int b) {
  unsigned int c = a + b;
  if (c > 0) {
    char result[c];
    result[c - 1] = 0;
  }
}

void call_null_pruning_symbols_1_Good() { null_pruning_symbols(10, 20); }

void call_null_pruning_symbols_2_Good() { null_pruning_symbols(0, 0); }

int unknown_function();

void call_null_pruning_symbols_3_Good_FP() {
  unsigned int c;
  if (unknown_function()) {
    c = 0;
  } else {
    c = 10;
  }
  null_pruning_symbols(c, 0);
}
