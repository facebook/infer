/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <stdlib.h>

struct delicious {
  int yum;
};

struct delicious *bake(struct delicious **cake) {
  int *zero = NULL;
  *zero = 3;
  return NULL;
}

struct delicious *skip_function_with_no_spec(void) {
  struct delicious *cake = NULL;
  int i;

  if(bake(&cake) == NULL) {
    return 0;
  }

  i = cake->yum;
  return cake;
}

extern struct delicious *bakery(struct delicious **cake);
extern struct delicious *bakery2(struct delicious **cake,
                                 struct delicious **pie);
extern struct delicious *returnPassByRef();

struct delicious *skip_external_function(void) {
  struct delicious *cake = NULL;
  int i;

  if(bakery(&cake) == NULL) {
    return 0;
  }

  i = cake->yum;
  return cake;
}

void by_ref_actual_already_in_footprint(struct delicious *param) {
  int i;
  struct delicious * ret = bakery(&param);
  i = param->yum;
}

void call_by_ref_actual_already_in_footprint_ok() {
  by_ref_actual_already_in_footprint(NULL); // should not report a warning
}

void by_ref_actual_already_in_footprint2(struct delicious *param) {
  int i;
  i = param->yum;  // should not report a warning
  struct delicious * ret = bakery(&param);
  i = param->yum;  // should not report a warning
}

void call_by_ref_actual_already_in_footprint_bad() {
  by_ref_actual_already_in_footprint2(NULL); // should report a warning
}

void passByRefTwice() {
  struct delicious *param;
  bakery2(&param, &param); // should not report a warning
  int i = param->yum;
}

struct delicious * returnPassByRef2() {
  struct delicious *param = NULL;
  bakery(&param);
  int i = param->yum; // should not report a warning
  return param;
}

void returnPassByRefDeref() {
  struct delicious *ret = returnPassByRef();
  ret->yum = 2; // should not report a warning
  free(ret);
}
