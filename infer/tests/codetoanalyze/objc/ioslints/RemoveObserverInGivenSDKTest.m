/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
