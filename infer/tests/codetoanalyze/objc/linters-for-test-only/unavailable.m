/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

@interface FooClass : NSObject

// this should be flagged via `has_unavailable_attribute`
+ (instancetype)new NS_UNAVAILABLE;
+ (instancetype)newWithFoo:(id<NSObject>)foo;

@end

@implementation FooClass
@end
