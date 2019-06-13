/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
