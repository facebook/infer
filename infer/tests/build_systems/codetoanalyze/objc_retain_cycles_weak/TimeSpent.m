/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import "TimeSpent.h"
#import "AnalyticsTimeSpent.h"
#import "ListAdapter.h"

@interface TimeSpent () {
  AnalyticsTimeSpent* _timeSpent;
}

@property(weak) TimeSpent* weakSelfRef;
@property(strong) TimeSpent* strongSelfRef;
@end

@implementation TimeSpent {
  ListAdapter* _listAdapter;
  TimeSpent* _strongSelfRef2;
}

- (instancetype)init {
  _timeSpent = [[AnalyticsTimeSpent alloc] initWithDelegate:self];
  return self;
}

- (instancetype)init_good {
  if (self = [super init]) {
    _listAdapter.dataSource = self;
  }
  return self;
}

- (instancetype)init_bad_ref {
  if (self = [super init]) {
    _weakSelfRef = self;
    _strongSelfRef = self;
  }
}

- (instancetype)init_bad_ref2 {
  if (self = [super init]) {
    _weakSelfRef = self;
    _strongSelfRef2 = self;
  }
}

- (instancetype)init_bad {
  if (self = [super init]) {
    _listAdapter.dataSourceStrong = self;
  }
  return self;
}

- (void)setAnalyticsTimeSpent:(AnalyticsTimeSpent*)timeSpent {
  _timeSpent = timeSpent;
}
@end

int retain_cycle_weak_good() {
  TimeSpent* ts = [TimeSpent new];
  return 0;
}

int retain_cycle_weak_bad() {
  TimeSpent* ts = [[TimeSpent alloc] init];
  AnalyticsTimeSpent* ats =
      [[AnalyticsTimeSpent alloc] initWithStrongDelegate:ts];
  [ts setAnalyticsTimeSpent:ats];
  return 0;
}

int retain_cycle_field_in_impl_bad(TimeSpent* ts) {
  AnalyticsTimeSpent* ats = [[AnalyticsTimeSpent alloc] initWithRecord:ts];
  [ts setAnalyticsTimeSpent:ats];
  return 0;
}
