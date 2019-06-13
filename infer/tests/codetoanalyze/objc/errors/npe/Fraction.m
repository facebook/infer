/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface Fraction : NSObject {
  int numerator;
  int denominator;
}

- (void)setNumerator:(int)n;
- (void)setDenominator:(int)d;

- (int)getNumerator;
- (int)getDenominator;
@end

@implementation Fraction

- (void)setNumerator:(int)n {
  numerator = n;
}

- (void)setDenominator:(int)d {
  denominator = d;
}

- (int)getNumerator {
  return numerator;
}

- (int)getDenominator {
  return denominator;
}
@end

Fraction* Fraction_create() {
  Fraction* f = NULL;
  return f;
}

int null_deref_objc_class() {
  Fraction* fraction = Fraction_create();
  [fraction setNumerator:5];
  return 0;
}

void test_virtual_call() {
  id* fraction = [Fraction alloc];
  // virtual call: the type of fraction is obtained from the allocation
  [fraction setNumerator:5];
  if ([fraction getNumerator] != 4) {
    // unreachable
    int* x = NULL;
    *x = 0;
  }
  [fraction release];
}
