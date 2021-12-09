/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

int isKindOfClassUnknown(Base* b) {
  if ([b isKindOfClass:[Derived class]]) {
    int* p = NULL;
    return *p;
  } else {
    return 0;
  }
}

int isKindOfClassBaseNoNPEGood() {
  Base* base = [[Base alloc] init];
  return isKindOfClassUnknown(base);
}

int isKindOfClassBaseNPEBad() {
  Base* derived = [[Derived alloc] init];
  return isKindOfClassUnknown(derived);
}
