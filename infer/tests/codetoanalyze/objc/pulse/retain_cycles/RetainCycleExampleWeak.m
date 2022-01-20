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

@property(nonatomic, strong) Adapter* strong_adapter;

@end

@implementation AFeed

@end

@interface RetainCycleExampleWeak : NSObject

@property(nonatomic, strong) AFeed* feed;

@end

@implementation RetainCycleExampleWeak

- (void)test_weak_adapter_no_cycle_good {
  _feed.adapter.feed = _feed;
}

- (void)test_strong_adapter_cycle_bad {
  _feed.strong_adapter.feed = _feed;
}

@end

void create_weak_cycle() {
  RetainCycleExampleWeak* r = [[RetainCycleExampleWeak alloc] init];
  r.feed = [[AFeed alloc] init];
  r.feed.adapter = [[Adapter alloc] init];
  [r test_weak_adapter_no_cycle_good];
}

// AFeed.dealloc has 18 disjuncts but RetainCycleExampleWeak.dealloc only has 6:
// - self is nil
// - ref count = 1:
//      - _feed is nil
//      - _feed is not nil and its ref count = 1 :
//              - _adapter and _strong_adapter are nil
//              - _adapter is not nil and its ref count = 1:
//                      - its _feed is nil
//                      - its _feed is not nil
// - ref count > 1
//
// Therefore, when we reach call RetainCycleExampleWeak.dealloc with _feed stuck
// in a retain cycle (cause by the call to test_strong_adapter_cycle_bad), we
// don't find any valid disjunct and end up with no Sat state: indeed, the
// disjunct for ref count = 1, _feed not nil and its ref count > 1 is missing
void create_strong_cycle_FN() {
  RetainCycleExampleWeak* r = [[RetainCycleExampleWeak alloc] init];
  r.feed = [[AFeed alloc] init];
  r.feed.strong_adapter = [[Adapter alloc] init];
  [r test_strong_adapter_cycle_bad];
}
