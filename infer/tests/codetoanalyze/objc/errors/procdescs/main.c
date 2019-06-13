/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
