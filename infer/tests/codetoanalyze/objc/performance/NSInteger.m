/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

void nsinteger_value_linear(NSInteger integer) {
  for (int count = 0; count < integer; count++) {
  }
}

void nsnumber_number_with_int_integer_value_constant() {
  int n = 4;
  NSNumber* number = [NSNumber numberWithInt:n];
  for (int i = 0; i < [number integerValue]; i++) {
  }
}
