/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface Autoreleasepool : NSObject

@end

@implementation Autoreleasepool

- (void)test {
  @autoreleasepool {
    int i = 0;
  }
}

- (void)test1 {
  for (int i = 0; i < 10; i++) {
    @autoreleasepool {
      int i = 0;
    }
  }
}

- (void)test2 {
  for (int i = 0; i < 10; i++) {
    @autoreleasepool {
      for (int j = 0; j < 10; j++) {
        int k = 0;
      }
    }
  }
}

@end
