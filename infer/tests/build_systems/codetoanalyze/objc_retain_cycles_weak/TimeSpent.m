/*
 * Copyright (c) 2018-present, Facebook, Inc.
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
@end

@implementation TimeSpent {
  ListAdapter* _listAdapter;
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
