/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include "MethodCall.h"
#include <stdlib.h>

int ProcdescMain() {
  MethodCall* call = [MethodCall alloc];
  int n = [call plusX:1 andY:3];
  int* x = malloc(sizeof(int));
  return n;
}

int call_nslog() {
  MethodCall* call = [MethodCall alloc];
  NSLog(@"%s", "printing");
  int* x = malloc(sizeof(int));
  return 0;
}
