/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

- (instancetype)withMetadata:(NSString*)name;

@end

void c_function(_Nonnull MyBlock block1,
                _Nonnull MyBlock block2,
                NSString* _Nonnull name) {
  block1(22);
  int my_var = 11;
  block2(33);
  int my_other_var = 12;
};
