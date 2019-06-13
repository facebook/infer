/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

typedef void (^MyBlock)();

@interface B : NSObject {
  int y;
  int x;
  int h;
}
+ (void)foo:(int)z and:(MyBlock)block;

@end

@implementation B

- (int)f {
  [B foo:h
      and:^{
        self->x = 5;
      }];
  return self->y;
}
@end
