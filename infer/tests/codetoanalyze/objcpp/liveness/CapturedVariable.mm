/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface A : NSObject

@end

typedef int (^MyBlock)(int);

@interface C : NSObject

+ (int)use_block:(MyBlock)block;

@end

@implementation A

+ (void)FP_block_captured_var_ok {
  const int captured = 3;
  [C use_block:^(int param) { // `captured` is not in the list of captured
                              // variables in objc++
    if (captured > param) {
      return captured;
    }
    return param;
  }];
};

@end
