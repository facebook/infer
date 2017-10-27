/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
