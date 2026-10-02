/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void cleanup_int(int* x) {}

void cleanup_char(char* x) {}

int scopes(int n) {
  __attribute__((cleanup(cleanup_int))) int x = 0;
  for (int i = 0; i < n; i++) {
    __attribute__((cleanup(cleanup_char))) char c = 'a';
    if (i == 1) {
      continue;
    }
    if (i == 2) {
      break;
    }
    if (i == 3) {
      return 1;
    }
  }
  __attribute__((cleanup(cleanup_int))) int y = 1;
  return x + y;
}

void no_return_stmt() {
  int unattributed = 0;
  __attribute__((cleanup(cleanup_int))) int x = 0;
}

int statement_expressions() {
  int r = ({
    __attribute__((cleanup(cleanup_int))) int x = 1;
    x;
  });
  return ({
    __attribute__((cleanup(cleanup_int))) int y = r;
    y;
  });
}

void gotos(int n) {
  __attribute__((cleanup(cleanup_int))) int x = 0;
again:;
  __attribute__((cleanup(cleanup_int))) int y = 1;
  {
    __attribute__((cleanup(cleanup_int))) int z = 2;
    if (n-- > 1) {
      goto again;
    }
    if (n > 0) {
      goto out;
    }
  }
out:
  return;
}
