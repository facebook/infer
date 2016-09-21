/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import "BlockVar.h"

@implementation BlockVar

+ (int)test {
  return 5;
}

+ (int)navigateToURLInBackground {
  int (^addBlock)(int a, int b) = ^(int a, int b) {
    int res = [self test];
    return a + b + res;
  };
  int x = addBlock(1, 2);
  int* p = 0;
  if (x == 8)
    return *p;
  else
    return x;
}

- (int)blockPostBad {
  int* x = NULL;
  int* (^my_block)(void) = ^() {
    return x;
  };
  return *my_block(); // should report null deref here
}

- (int)blockPostOk {
  int i = 7;
  int* x = &i;
  int* (^my_block)(void) = ^() {
    return x;
  };
  return *my_block(); // should not report null deref here
}

- (int)capturedNullDeref {
  int* x = NULL;
  int (^my_block)(void) = ^() {
    return *x;
  };
  return my_block(); // should report null deref here
}

- (int)capturedNoNullDeref {
  int i = 5;
  int* x = &i;
  int (^my_block)(void) = ^() {
    return *x;
  };
  x = NULL;
  return my_block(); // should not report null deref here
}

@end
