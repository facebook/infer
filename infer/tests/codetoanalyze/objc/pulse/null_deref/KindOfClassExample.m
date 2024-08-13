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

int* nullIfIsDerived(Base* b, int* p) {
  if ([b isKindOfClass:[Derived class]]) {
    return NULL;
  } else {
    return p;
  }
}

void isKindOfClassBaseNoNPEGood(int* p) {
  Base* base = [[Base alloc] init];
  int* q = nullIfIsDerived(base, p);
  return *q;
}

int isKindOfClassBaseNPEBad(int* p) {
  Base* derived = [[Derived alloc] init];
  int* q = nullIfIsDerived(derived, p);
  return *q;
}
