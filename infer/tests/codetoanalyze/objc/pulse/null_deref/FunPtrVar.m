/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface FunPtrVar : NSObject

@end

@implementation FunPtrVar

int add5(int a, int b) { return a + b + 5; }

+ (void)callFunPtrNPEBad {
  int (*addFunPtr)(int, int) = &add5;
  int x = (*addFunPtr)(1, 2);
  int* p = NULL;
  if (x == 8) {
    *p = 42;
  }
}

int* intptr_id(int* x) { return x; }

- (int)funptrPostNullDerefBad {
  int* x = NULL;
  int* (*my_funptr)(int*) = &intptr_id;
  return *(*my_funptr)(x); // should report null deref here
}

- (int)funptrPostGood {
  int i = 7;
  int* x = &i;
  int* (*my_funptr)(int*) = &intptr_id;
  return *(*my_funptr)(x); // should not report null deref here
}

int deref_intptr(int* x) { return *x; }

- (int)argNullDerefBad {
  int* x = NULL;
  int (*my_funptr)(int*) = &deref_intptr;
  return (*my_funptr)(x); // should report null deref here
}

- (int)argNoNullDerefGood {
  int i = 5;
  int* x = &i;
  int (*my_funptr)(int*) = &deref_intptr;
  return (*my_funptr)(x); // should not report null deref here
}

@end
