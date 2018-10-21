/*
 * Copyright (c) 2017-present, Facebook, Inc.
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

void call_fromHex_200_Good_FP() {
  char arr[16];
  int idx = fromHex(200);
  if (idx >= 0) {
    arr[idx] = 'H';
  }
}

void call_fromHex2_sym_Good(char c) {
  char arr[17];
  int idx = fromHex(c);
  arr[idx + 1] = 'H';
}

void call_fromHex2_200_Good_FP() {
  char arr[17];
  int idx = fromHex(200);
  arr[idx + 1] = 'H';
}
