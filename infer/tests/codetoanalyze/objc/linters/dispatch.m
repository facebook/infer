/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface A : NSObject
@end

@implementation A

+ (instancetype)sharedInstanceOK {
  static id _sharedInstance = nil;
  static dispatch_once_t onceTokenA;
  dispatch_once(&onceTokenA, ^{
    _sharedInstance = [[self alloc] init];
  });

  return _sharedInstance;
}

@end

@interface B : NSObject
@end

@implementation B

+ (instancetype)sharedInstanceBAD {
  static id _sharedInstance = nil;
  dispatch_once_t onceTokenB;
  dispatch_once(&onceTokenB, ^{
    _sharedInstance = [[self alloc] init];
  });

  return _sharedInstance;
}

@end
