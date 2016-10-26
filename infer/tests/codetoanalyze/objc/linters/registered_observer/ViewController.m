/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Cocoa/Cocoa.h>
@interface ViewController : NSViewController
@end

@implementation ViewController

- (instancetype)init {

  [[NSNotificationCenter defaultCenter] addObserver:self
                                           selector:@selector(fired)
                                               name:@"some_notification"
                                             object:nil];
  return self;
}

- (void)fired {
  NSLog(@"This codepath fired");
}

- (void)invalidate1 {
  [[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)invalidate2 {
  [[NSNotificationCenter defaultCenter] removeObserver:self
                                                  name:@"some_notification"
                                                object:nil];
}
@end

int fooError() {
  ViewController* vc = [[ViewController alloc] init];
  ViewController* vc2;
  vc2 = vc;
  [vc fired];
  return 0;
}

int fooOK1() {
  ViewController* vc = [[ViewController alloc] init];
  ViewController* vc2;
  vc2 = vc;
  [vc fired];
  [vc invalidate1];
  return 0;
}

int fooOK2() {
  ViewController* vc = [[ViewController alloc] init];
  ViewController* vc2;
  vc2 = vc;
  [vc fired];
  [vc invalidate2];
  return 0;
}

int barError() {
  // ViewController* vc = [[ViewController alloc] init];
  //[vc fired];

  // vc = [[ViewController alloc] init];
  return 0;
}

int barOK1() {
  ViewController* vc = [[ViewController alloc] init];
  [vc invalidate1];
  vc = [[ViewController alloc] init];
  [vc invalidate1];
  return 0;
}

int barOK2() {
  ViewController* vc = [[ViewController alloc] init];
  [vc invalidate2];
  vc = [[ViewController alloc] init];
  [vc invalidate1];
  return 0;
}
