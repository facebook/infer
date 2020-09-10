/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

@interface NSObject
@end
@implementation NSObject
@end

@interface available_expression : NSObject

@end

@implementation available_expression

- (void)test_no_bug:(int)n and:(available_expression *)data {
  if (@available(macOS 10.13, iOS 11.0, *)) {
  }

  if (__builtin_available(macos 10.10, ios 8, *)) {
  }
}

@end
