/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import "Ctor.mm"
#import <mutex>

@interface CtorInherit : Ctor
- (instancetype)init;
- (void)writeZero;
@end

@implementation CtorInherit

- (instancetype)init {
  if (!(self = [super init])) {
    return nil;
  }
  return self;
}

- (void)write_zero_ok {
  [self write:0];
}
@end
