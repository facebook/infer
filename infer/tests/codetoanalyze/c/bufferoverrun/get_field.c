/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "my_typedef.h"

struct st {
  int field;
};

int get_field(t* x) { return x->field; }

void call_get_field_cond_Bad() {
  int a[5];
  t x = {0};
  if (get_field_wrapper(&x)) {
    a[10] = 0;
  } else {
    a[10] = 0;
  }
}

void FP_call_get_field_Good() {
  int a[5];
  t x = {0};
  a[get_field_wrapper(&x)] = 0;
}

void call_get_field_Bad() {
  int a[5];
  t x = {10};
  a[get_field_wrapper(&x)] = 0;
}
