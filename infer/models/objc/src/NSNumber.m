/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import "NSNumber.h"

@implementation NSNumber

- (id)initWithInt:(int)v {
  self->value = (double)v;
  return self;
}

- (id)initWithFloat:(float)v {
  return [self initWithInt:(int)v];
}

- (id)initWithBool:(BOOL)v {
  return [self initWithInt:(int)v];
}

- (id)initWithDouble:(double)v {
  return [self initWithInt:(int)v];
}

- (id)initWithUnsignedInteger:(NSUInteger)v {
  return [self initWithInt:(int)v];
}

+ (NSNumber*)numberWithInt:(int)value {
  // using alloc as the documentation doesn't say it may return nil
  NSNumber* number = [self alloc];
  return [number initWithInt:value];
}

+ (NSNumber*)numberWithChar:(char)value {
  return [NSNumber numberWithInt:(int)value];
}

+ (NSNumber*)numberWithShort:(short)value {
  return [NSNumber numberWithInt:(int)value];
}

+ (NSNumber*)numberWithBool:(BOOL)value {
  return [NSNumber numberWithInt:(int)value];
}

+ (NSNumber*)numberWithInteger:(NSInteger)value {
  return [NSNumber numberWithInt:(int)value];
}

+ (NSNumber*)numberWithFloat:(float)value {
  return [NSNumber numberWithInt:(int)value];
}

+ (NSNumber*)numberWithDouble:(double)value {
  return [NSNumber numberWithInt:(int)value];
}

+ (NSNumber*)numberWithLong:(long)value {
  return [NSNumber numberWithInt:(int)value];
}

+ (NSNumber*)numberWithLongLong:(long long)value {
  return [NSNumber numberWithInt:(int)value];
}

+ (NSNumber*)numberWithUnsignedInt:(unsigned int)value {
  return [NSNumber numberWithInt:(int)value];
}

+ (NSNumber*)numberWithUnsignedChar:(unsigned char)value {
  return [NSNumber numberWithInt:(int)value];
}

+ (NSNumber*)numberWithUnsignedShort:(unsigned short)value {
  return [NSNumber numberWithInt:(int)value];
}

+ (NSNumber*)numberWithUnsignedInteger:(NSUInteger)value {
  return [NSNumber numberWithInt:(int)value];
}

+ (NSNumber*)numberWithUnsignedLong:(unsigned long)value {
  return [NSNumber numberWithInt:(int)value];
}

+ (NSNumber*)numberWithUnsignedLongLong:(unsigned long long)value {
  return [NSNumber numberWithInt:(int)value];
}

@end
