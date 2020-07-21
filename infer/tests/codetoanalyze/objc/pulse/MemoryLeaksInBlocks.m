/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>
#include <stdlib.h>

int block_captured_var_leak_bad() {
  int* x = malloc(sizeof(int));
  *x = 2;
  int (^blk)(void) = ^() {
    return *x;
  };
  return blk();
}

int block_free_ok() {
  int* x = malloc(sizeof(int));
  *x = 2;
  int (^blk)(void) = ^() {
    int i = *x;
    free(x);
    return i;
  };
  return blk();
}
