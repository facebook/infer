/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import "Boxing.h"

@implementation Boxing

- (NSNumber*)getIntExp {
  int x = 4;
  int y = 5;
  NSNumber *n = [NSNumber numberWithInt: x+y];
  return @(x+y);
}

- (NSNumber*)getInt {
  NSNumber *n = [NSNumber numberWithInt: 5];
  return @5;
}

- (NSNumber*)getFloat {
  NSNumber *n = [NSNumber numberWithFloat: 1.5f];
  return @1.5f;
}

- (NSNumber*)getDouble {
  NSNumber *n = [NSNumber numberWithDouble: 1.5];
  return @1.5;
}

- (NSNumber*)getBool {
  NSNumber *n = [NSNumber numberWithBool: YES];
  return @YES;
}

- (NSString*)getS{
  NSString *s = @(strdup("hello world")) ;
  return [NSString stringWithUTF8String: "hello world"];;
}

@end
