/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import "NSRunLoop.h"

@implementation NSRunLoop

+ (NSRunLoop*)currentRunLoop {

  return [NSRunLoop alloc];
};

+ (NSRunLoop*)mainRunLoop {

  return [NSRunLoop alloc];
};

- (void)acceptInputForMode:(NSString*)mode beforeDate:(NSDate*)limit_date {
};

- (void)addTimer:(NSTimer*)timer forMode:(NSString*)mode {
};

- (NSString*)currentMode {
  return @"";
};

- (NSDate*)limitDateForMode:(NSString*)mode {

  return [NSDate alloc];
};

- (void)run {
};

- (BOOL)runMode:(NSString*)mode beforeDate:(NSDate*)date {

  int res;
  return res;
};

- (void)runUntilDate:(NSDate*)date {
};

@end
