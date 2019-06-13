/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSDate.h>

@class NSInvocation;

@interface NSTimer : NSObject {
  NSTimeInterval _interval;
  id _info;
  id _target;
  SEL _selector;
  unsigned _repeats : 2;
  unsigned _timer_filler : 6;
 @public
  NSDate* _fireDate;
  BOOL _is_valid;
}

+ (NSTimer*)scheduledTimerWithTimeInterval:(NSTimeInterval)ti
                                invocation:(NSInvocation*)invocation
                                   repeats:(BOOL)f;
+ (NSTimer*)scheduledTimerWithTimeInterval:(NSTimeInterval)ti
                                    target:(id)object
                                  selector:(SEL)selector
                                  userInfo:(id)info
                                   repeats:(BOOL)f;
+ (NSTimer*)timerWithTimeInterval:(NSTimeInterval)ti
                       invocation:(NSInvocation*)invocation
                          repeats:(BOOL)f;
+ (NSTimer*)timerWithTimeInterval:(NSTimeInterval)ti
                           target:(id)object
                         selector:(SEL)selector
                         userInfo:(id)info
                          repeats:(BOOL)f;

- (void)fire;
- (NSDate*)fireDate;
- (id)initWithFireDate:(NSDate*)date
              interval:(NSTimeInterval)seconds
                target:(id)target
              selector:(SEL)aSelector
              userInfo:(id)userInfo
               repeats:(BOOL)repeats;
- (void)invalidate;
- (BOOL)isValid;
- (void)setFireDate:(NSDate*)date;
- (NSTimeInterval)timeInterval;
- (id)userInfo;

@end
