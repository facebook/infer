/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

@interface Available_expr : NSObject

@end

@implementation Available_expr

- (int)test_no_bug {
  int* p = NULL;
  if (@available(macOS 10.13, iOS 11.0, *)) {
    return *p;
  }
  return 0;
}

@end
