/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>
#include <dispatch/dispatch.h>

@interface A : NSObject
- (int)foo:(int&)y;
@end

@implementation A

- (int)foo:(int&)y {
  dispatch_async(dispatch_get_main_queue(), ^{
    int a = y; // Error
    return;
  });
  return 1;
}

- (int)foo2:(int&)y {
  const int copied_y = y;
  dispatch_async(dispatch_get_main_queue(), ^{
    int a = copied_y; // OK
    return;
  });
  return 1;
}

- (int)foo3:(int&)y param2:(int&)z {
  dispatch_async(dispatch_get_main_queue(), ^{
    int a = y; // Error
    int b = z; // Error
    return;
  });
  return 1;
}

@end
