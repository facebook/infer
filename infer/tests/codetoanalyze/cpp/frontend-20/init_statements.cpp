/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace init_statements {

struct X {
  ~X() {}
};

int range_for_init(int k) {
  int arr[3] = {1, 2, 3};
  int sum = 0;
  for (int base = k; int v : arr) {
    sum += base + v;
  }
  return sum;
}

int range_for_over_init_variable() {
  int sum = 0;
  for (int arr[2] = {1, 2}; int v : arr) {
    sum += v;
  }
  return sum;
}

int range_for_init_destructor(bool b) {
  int arr[3] = {1, 2, 3};
  int sum = 0;
  for (X x; int v : arr) {
    if (b) {
      break;
    }
    sum += v;
  }
  return sum;
}

int switch_init_destructor(int k) {
  switch (X x; k) {
    case 0:
      return 0;
    default:
      break;
  }
  return 1;
}

int switch_init_destructor_on_continue(int n, int k) {
  int sum = 0;
  for (int i = 0; i < n; i++) {
    switch (X x; k) {
      case 0:
        continue;
      default:
        sum += i;
    }
  }
  return sum;
}

int switch_init_discarded_load(int* p, int k) {
  switch ((void)*p; k) {
    case 0:
      return 0;
    default:
      return 1;
  }
}

int if_init_typedef(int k) {
  if (typedef int T; k == 0) {
    return T(1);
  }
  return 0;
}

} // namespace init_statements
