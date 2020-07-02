/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

NSInteger block_multiply_array_linear_FN(NSArray* array) {
  NSInteger (^sum_array)(NSArray*) = ^(NSArray* array) {
    NSInteger n = 0;
    for (id value in array) {
      n += [value integerValue];
    }
    return n;
  };

  return sum_array(array);
}
