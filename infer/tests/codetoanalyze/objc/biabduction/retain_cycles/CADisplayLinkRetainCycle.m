/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
  _displayLink = [CADisplayLink displayLinkWithTarget:self
                                             selector:@selector(bla)];

  return self;
}

- (void)retain_cycle_weak_good {
  __weak __typeof__(self) weakSelf = self;
  _displayLink =
      [CADisplayLink displayLinkWithTarget:weakSelf
                                  selector:@selector(handleDisplayLink:)];
}

- (void)bla {
};

- (void)invalidate {
  // unregister displayLink target
  [_displayLink invalidate];
};

@end

void test_retain_cycle_bad_FN() {

  CADisplay* a = [[CADisplay alloc] init];
  CADisplay* b = a;
}

void test_retain_cycle_good() {

  CADisplay* a = [[CADisplay alloc] init];
  [a invalidate]; // break the cycle
}
