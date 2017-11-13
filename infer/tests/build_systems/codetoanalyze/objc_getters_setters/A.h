/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#import <Foundation/NSObject.h>
#import <Foundation/NSData.h>

@interface A : NSObject

typedef void (^MyBlock)(int x);

@property(nullable, nonatomic, copy) NSData* metadata;

- (int)getX;

+ (void)foo:(int)z
         and:(_Nonnull MyBlock)block1
    and_also:(_Nonnull MyBlock)block2
         and:(nullable NSString*)name;

@end

void c_function(_Nonnull MyBlock block1,
                _Nonnull MyBlock block2,
                NSString* _Nonnull name) {
  block1(22);
  int my_var = 11;
  block2(33);
  int my_other_var = 12;
};
