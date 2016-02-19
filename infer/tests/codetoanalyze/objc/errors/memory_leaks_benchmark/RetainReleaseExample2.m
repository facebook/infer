/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>

@interface A : NSObject

@end

@implementation A

@end

void __objc_release(A*); // infer builtin
A* g;

// no leak
A* test() {
  A* a = [[A alloc] init];
  [a retain];
  [a release];

  return a;
}

// no leak
void test2() {

  A* b = test();
  g = b;
}

// leak
void test3() { A* b = test(); }

// no leak
void test4() {

  A* b = test();
  [b release];
}

// No leak
void test5() {
  A* a = [[A alloc] init];
  [a release];
}

// leak
void test6() {
  A* a = [[A alloc] init];
  [a retain];
  [a release];
}

// Creates specs
void test7(A* a) {
  if (a)
    __objc_release(a);
}
