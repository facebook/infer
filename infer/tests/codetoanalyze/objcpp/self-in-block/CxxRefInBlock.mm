/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>
#include <dispatch/dispatch.h>

@interface CxxRefInBlock : NSObject
- (int)foo:(int&)y;
@end

@implementation CxxRefInBlock

- (int)ref_captured_in_escaping_block_bad:(int&)y and:(int*)ptr {
  dispatch_async(dispatch_get_main_queue(), ^{
    int a = y;
    int i = *ptr;
    return;
  });
  return 1;
}

- (int)ref_captured_in_no_escaping_block_good:(int&)y {
  dispatch_sync(dispatch_get_main_queue(), ^{
    int a = y;
    return;
  });
  return 1;
}

- (int)multiple_refs_captured_in_escaping_block_bad:(int&)y param2:(int&)z {
  dispatch_async(dispatch_get_main_queue(), ^{
    int a = y;
    int b = z;
    return;
  });
  return 1;
}

- (int)ref_copied_good:(int&)y {
  const int copied_y = y;
  dispatch_async(dispatch_get_main_queue(), ^{
    int a = copied_y;
    return;
  });
  return 1;
}

@end
