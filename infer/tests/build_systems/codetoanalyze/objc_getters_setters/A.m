/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import "A.h"

@implementation A {

  int _x;
  NSString* _name;
}

- (instancetype)withMetadata:(NSString*)name {
  self->_x = 5;
  self->_name = name;
  return self;
}

- (int)getX {
  return _x;
}

+ (void)foo:(int)z
         and:(_Nonnull MyBlock)block1
    and_also:(_Nonnull MyBlock)block2
         and:(nullable NSString*)name {
  block1(22);
  int my_var = 11;
  block2(33);
  int my_other_var = 12;
}

@end
