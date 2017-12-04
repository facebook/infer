/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#import <Foundation/NSObject.h>

@class AFeed;

@interface Adapter : NSObject

@property(nonatomic, strong) AFeed* feed;

@end

@implementation Adapter

@end

@interface AFeed : NSObject

@property(nonatomic, weak) Adapter* adapter;

@end

@implementation AFeed

@end

@interface RetainCycleExampleWeak : NSObject

@property(nonatomic, strong) AFeed* feed;

@end

@implementation RetainCycleExampleWeak

- (void)test {
  _feed.adapter.feed = _feed;
}

- (void)main1 {
  RetainCycleExampleWeak* b = [RetainCycleExampleWeak new];
  [b test];
}

@end
