/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#import "AnalyticsTimeSpent.h"
#import "TimeSpent.h"

@implementation AnalyticsTimeSpent

@synthesize delegate;

- (instancetype)initWithDelegate:(id)delegateObject {
  [self setDelegate:delegateObject];
  return self;
}

- (instancetype)initWithStrongDelegate:(id)delegateObject {
  _strong_delegate = delegateObject;
  return self;
}

@end
