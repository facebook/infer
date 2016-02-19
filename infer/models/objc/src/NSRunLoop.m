/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import "NSRunLoop.h"

@implementation NSRunLoop

+ (NSRunLoop*)currentRunLoop {

  return [NSRunLoop alloc];
};

+ (NSRunLoop*)mainRunLoop {

  return [NSRunLoop alloc];
};

- (void)acceptInputForMode:(NSString*)mode beforeDate:(NSDate*)limit_date{};

- (void)addTimer:(NSTimer*)timer
         forMode:(NSString*)mode{

                 };

- (NSString*)currentMode {
  return @"";
};

- (NSDate*)limitDateForMode:(NSString*)mode {

  return [NSDate alloc];
};

- (void)run{};

- (BOOL)runMode:(NSString*)mode beforeDate:(NSDate*)date {

  int res;
  return res;
};

- (void)runUntilDate:(NSDate*)date{};

@end
