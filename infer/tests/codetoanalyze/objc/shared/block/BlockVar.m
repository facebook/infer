/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
