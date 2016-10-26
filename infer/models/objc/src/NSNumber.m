/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import "NSNumber.h"

@implementation NSNumber

+ (NSNumber*)numberWithInt:(int)value {
  // using alloc as the documentation doesn't say it may return nil
  NSNumber* number = [self alloc];
  return [number initWithInt:value];
}

- (id)initWithInt:(int)v {
  self->value = (double)v;
  return self;
}

+ (NSNumber*)numberWithFloat:(float)value {
  // using alloc as the documentation doesn't say it may return nil
  NSNumber* number = [NSNumber alloc];
  return [number initWithInt:value];
}

- (id)initWithFloat:(float)v {
  self->value = (double)v;
  return self;
}

+ (NSNumber*)numberWithDouble:(double)value {
  // using alloc as the documentation doesn't say it may return nil
  NSNumber* number = [self alloc];
  return [number initWithInt:value];
}

- (id)initWithDouble:(double)v {
  self->value = v;
  return self;
}

+ (NSNumber*)numberWithBool:(BOOL)value {
  // using alloc as the documentation doesn't say it may return nil
  NSNumber* number = [self alloc];
  return [number initWithBool:value];
}

- (id)initWithBool:(BOOL)v {
  self->value = (double)v;
  return self;
}

- (id)initWithUnsignedInteger:(NSUInteger)v {
  self->value = v;
  return self;
}

+ (NSNumber*)numberWithUnsignedInteger:(NSUInteger)value {
  NSNumber* number = [self alloc];
  return [number initWithUnsignedInteger:value];
}

@end
