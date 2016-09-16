/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>

@interface RR2 : NSObject

@end

@implementation RR2

- init {
  return self;
}

@end

void __objc_release(RR2*); // infer builtin
RR2* g;

// no leak
RR2* retain_release2_test() {
  RR2* a = [[RR2 alloc] init];
  [a retain];
  [a release];

  return a;
}

// no leak
void retain_release2_test2() {

  RR2* b = retain_release2_test();
  g = b;
}

// leak
void test3() { RR2* b = retain_release2_test(); }

// no leak
void test4() {

  RR2* b = retain_release2_test();
  [b release];
}

// No leak
void test5() {
  RR2* a = [[RR2 alloc] init];
  [a release];
}

// leak
void test6() {
  RR2* a = [[RR2 alloc] init];
  [a retain];
  [a release];
}

// Creates specs
void test7(RR2* a) {
  if (a)
    __objc_release(a);
}
