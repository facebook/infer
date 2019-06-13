/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import "NSTimer.h"
#import <Foundation/NSDate.h>

// Note: we do not link timers to NSRunLoop as this is irrelevant for
// our models. If at some point becomes important it needs to be changed.

void free(void* ptr);

@implementation NSTimer

+ (NSTimer*)timerWithTimeInterval:(NSTimeInterval)seconds
                       invocation:(NSInvocation*)invocation
                          repeats:(BOOL)f {
  NSTimer* t = [self alloc];
  if (t) {
    t->_interval = seconds;
    t->_fireDate = [NSDate alloc];
    t->_is_valid = YES;
    t->_target = invocation;
    t->_repeats = f;
  }
  return t;
}

+ (NSTimer*)timerWithTimeInterval:(NSTimeInterval)seconds
                           target:(id)object
                         selector:(SEL)selector
                         userInfo:(id)info
                          repeats:(BOOL)f {
  NSDate* d = [NSDate alloc];
  return [[self alloc] initWithFireDate:d
                               interval:seconds
                                 target:object
                               selector:selector
                               userInfo:info
                                repeats:f];
}

+ (NSTimer*)scheduledTimerWithTimeInterval:(NSTimeInterval)ti
                                invocation:(NSInvocation*)invocation
                                   repeats:(BOOL)f {
  NSTimer* t = [self timerWithTimeInterval:ti invocation:invocation repeats:f];
  return t;
}

+ (NSTimer*)scheduledTimerWithTimeInterval:(NSTimeInterval)ti
                                    target:(id)object
                                  selector:(SEL)selector
                                  userInfo:(id)info
                                   repeats:(BOOL)f {
  NSTimer* t = [self timerWithTimeInterval:ti
                                    target:object
                                  selector:selector
                                  userInfo:info
                                   repeats:f];
  return t;
}

- (id)initWithFireDate:(NSDate*)date
              interval:(NSTimeInterval)seconds
                target:(id)object
              selector:(SEL)selector
              userInfo:(id)info
               repeats:(BOOL)f {
  _interval = seconds;
  _fireDate = date;
  _is_valid = YES;
  _selector = selector;
  _target = object;
  _info = info;
  _repeats = f;

  return self;
}

- (void)dealloc {
  _fireDate = nil;
  free(self);
}

- (NSString*)description;
{ return [NSString alloc]; }

// Abstract everything except making the timer invalid if
// cannot repeat
- (void)fire {
  if (!_repeats)
    _is_valid = NO;
}

- (void)invalidate {
  _is_valid = NO;
}

- (BOOL)isValid {
  return _is_valid;
}

- (NSDate*)fireDate {

  return _fireDate;
}

- (NSTimeInterval)timeInterval {
  return _interval;
}

- (id)userInfo {
  return _info;
}

- (void)setFireDate:(NSDate*)date;
{ _fireDate = date; }

- (int)compare:(NSTimer*)anotherTimer {
  int res;
  return res;
}

@end
