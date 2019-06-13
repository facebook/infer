/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

NS_ASSUME_NONNULL_BEGIN

@interface Blah : NSObject

+ (instancetype)someMethod;

@end

@implementation Blah

+ (instancetype)someMethod {
  return nil;
}

@end

NS_ASSUME_NONNULL_END
