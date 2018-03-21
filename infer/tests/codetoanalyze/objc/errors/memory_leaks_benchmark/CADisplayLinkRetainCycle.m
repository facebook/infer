/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>
#import <QuartzCore/QuartzCore.h>

@interface CADisplay : NSObject
@property(nonatomic, strong) CADisplayLink* displayLink;

- (void)bla;
- (void)invalidate;
@end

@implementation CADisplay

- (instancetype)init {
  _displayLink =
      [CADisplayLink displayLinkWithTarget:self selector:@selector(bla)];

  return self;
}

- (void)cycle_weak_good {
  __weak __typeof__(self) weakSelf = self;
  _displayLink =
      [CADisplayLink displayLinkWithTarget:weakSelf
                                  selector:@selector(handleDisplayLink:)];
}

- (void)bla{};

- (void)invalidate {
  // unregister displayLink target
  [_displayLink invalidate];
};

@end

void testCycle() {

  CADisplay* a = [[CADisplay alloc] init];
  CADisplay* b = a;
}

void testNoCycle() {

  CADisplay* a = [[CADisplay alloc] init];
  [a invalidate]; // break the cycle
}
