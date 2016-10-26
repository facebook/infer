/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>

@interface Base : NSObject

+ (int)returnsZero1:(Base*)b;

@end

@implementation Base

- init {
  return self;
}

+ (int)returnsZero1:(Base*)b {
  if ([b isKindOfClass:[self class]]) {
    return 0;
  } else {
    return 1;
  }
}

@end

@interface Derived : Base

@end

@implementation Derived

- init {
  return self;
}

@end

int returnsZero2(Base* b) {
  if ([b isKindOfClass:[Derived class]]) {
    return 1;
  } else {
    return 0;
  }
}

int shouldThrowDivideByZero1() {
  Base* base = [[Base alloc] init];
  return 1 / [Base returnsZero1:base];
}

int shouldThrowDivideByZero2() {
  Base* base = [[Base alloc] init];
  return 1 / returnsZero2(base);
}

int shouldThrowDivideByZero3() {
  Base* b = [[Derived alloc] init];
  if ([b isKindOfClass:[Derived class]]) {
    return 1 / 0;
  } else {
    return 0;
  }
}
