/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface BlockVar : NSObject

@end

@implementation BlockVar

+ (void)callBlockNPEBad {
  int (^addBlock)(int a, int b) = ^(int a, int b) {
    return a + b + 5;
  };
  int x = addBlock(1, 2);
  int* p = NULL;
  if (x == 8) {
    *p = 42;
  }
}

- (int)blockPostNullDerefBad {
  int* x = NULL;
  int* (^my_block)(void) = ^() {
    return x;
  };
  return *my_block(); // should report null deref here
}

- (int)blockPostGood {
  int i = 7;
  int* x = &i;
  int* (^my_block)(void) = ^() {
    return x;
  };
  return *my_block(); // should not report null deref here
}

- (int)capturedNullDerefBad {
  int* x = NULL;
  int (^my_block)(void) = ^() {
    return *x;
  };
  return my_block(); // should report null deref here
}

- (int)capturedNoNullDerefGood {
  int i = 5;
  int* x = &i;
  int (^my_block)(void) = ^() {
    return *x;
  };
  x = NULL;
  return my_block(); // should not report null deref here
}

@end
