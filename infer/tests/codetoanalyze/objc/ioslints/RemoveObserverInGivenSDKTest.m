/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface RemoveObserverInGivenSDKTest2 : NSObject

@property(strong) NSNotificationCenter* nc;

@end

@implementation RemoveObserverInGivenSDKTest2

- (void)foo:(NSMutableDictionary*)dict {
  self.nc = [NSNotificationCenter defaultCenter];
  [self.nc addObserver:self selector:@selector(foo:) name:nil object:nil];
}

@end
