/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int simple_init_div1() {
  if (int a = 1) {
    return 1 / a;
  }
}

int simple_init_div0() {
  if (int a = 0) {
    return a;
  } else {
    return 1 / a;
  }
}

int simple_inif_elseif_div0() {
  if (int a = 0) {
    return 1;
  } else if (int b = 0) {
    return 1;
  } else {
    return 1 / (a + b);
  }
}

int get1() { return 1; }

int function_call_init_div0() {
  if (int a = get1()) {
    return 1 / (a - 1);
  }
}

int conditional_init_div0() {
  if (int a = 1 ? 1 : 0) {
    return 1 / (a - 1);
  }
}

int reference_init_div0() {
  int r = 1;
  if (int& a = r) {
    a = 0;
  }
  return 1 / r;
}

int simple_init_null_deref() {
  if (int* p = nullptr) {
    return 1;
  } else {
    return *p;
  }
}
