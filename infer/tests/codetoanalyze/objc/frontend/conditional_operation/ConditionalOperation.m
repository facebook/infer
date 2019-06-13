/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface A : NSObject

- (int)test4:(int)x;
@end

@implementation A

- (int)test4:(int)x {
  return x;
}

- (int)test5:(BOOL)b {
  return [self test4:(b ? b : 1)];
}

@end
