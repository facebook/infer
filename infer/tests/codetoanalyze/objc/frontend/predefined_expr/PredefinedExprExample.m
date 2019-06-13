/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface A : NSObject

@end

@implementation A

- (void)testPrettyFunction {
  NSLog(@"%s", __PRETTY_FUNCTION__);
}

- (void)testFunction {
  NSLog(@"%s", __FUNCTION__);
}

- (void)testFunct {
  NSLog(@"%s", __func__);
}

@end
