/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSDate.h>
#import <Foundation/NSObject.h>

@class NSTimer;

@interface NSRunLoop : NSObject

+ (NSRunLoop*)currentRunLoop;
+ (NSRunLoop*)mainRunLoop;

- (void)acceptInputForMode:(NSString*)mode beforeDate:(NSDate*)limit_date;

- (void)addTimer:(NSTimer*)timer forMode:(NSString*)mode;

- (NSString*)currentMode;

- (NSDate*)limitDateForMode:(NSString*)mode;

- (void)run;

- (BOOL)runMode:(NSString*)mode beforeDate:(NSDate*)date;

- (void)runUntilDate:(NSDate*)date;

@end
