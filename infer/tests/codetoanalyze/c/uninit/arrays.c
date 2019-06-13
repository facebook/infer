/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int array_good1_FP() {
  int a[10];
  for (int i = 0; i < 10; i++) {
    a[i] = 0;
  }
  return a[2];
}

int array_bad() {
  int a[10];
  for (int i = 0; i < 5; i++) {
    a[i] = 0;
  }
  return a[9];
}

int nondet();

int array_good2_FP() {
  int* a[10];
  int i = 0;
  while (i < 10) {
    if (nondet()) {
      a[i] = 0;
      i++;
    }
  }
  return a[2];
}

int array_good3_FP(int m) {
  int* a[1000];
  int i = 0;
  while (i < 1000 && i < m) {
    if (nondet()) {
      a[i] = 0;
      m--;
    }
  }
  return i > 0 ? a[i - 1] : 42;
}

void mayinit_vpp(const void**);
void use_vp(void*);

void* call_to_mayinit_and_return_good() {
  void* obj[10];
  mayinit_vpp((const void**)obj);
  return obj[0];
}

void call_to_mayinit_and_call_use_good() {
  void* obj[10];
  mayinit_vpp((const void**)obj);
  use_vp(obj[0]);
}
