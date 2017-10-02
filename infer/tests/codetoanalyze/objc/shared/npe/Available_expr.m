/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
