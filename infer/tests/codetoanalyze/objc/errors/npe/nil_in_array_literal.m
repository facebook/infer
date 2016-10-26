/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

@interface Arr : NSObject {
}
@end

@implementation Arr

- (void)noProblem {
  NSArray* foo = @[ @"aaa", @"bbb" ];
  // check that array literals create valid objects
  NSArray* foofoo = @[ foo ];
  NSArray* bar = [NSArray arrayWithObject:@"ccc"];
  // test return value of arrayWithObject to avoid RETURN_VALUE_IGNORED report
  if (bar)
    return;
}

- (void)nilInArrayLiteral0 {
  NSString* str = nil;

  // nil argument in array literal crashes
  NSArray* foo = @[ str ];
}

- (void)nilInArrayLiteral1 {
  NSString* str = nil;

  // nil argument in array literal crashes
  NSArray* foo = @[ str, @"bbb" ];
}

- (void)nilInArrayLiteral2 {
  NSString* str = nil;

  // nil argument in array literal crashes
  NSArray* foo = @[ @"aaa", str, @"bbb" ];
}

- (void)nilInArrayLiteral3 {
  NSString* str = nil;

  // nil argument in array literal crashes
  NSArray* foo = @[ @"aaa", @"bbb", str ];
}

- (NSArray*)nilInArrayWithObject {
  NSString* str = nil;

  // nil argument in arrayWithObject crashes
  NSArray* foo = [NSArray arrayWithObject:str];

  return foo;
}

@end

int ArrMain() {
  Arr* a = [Arr alloc];
  [a noProblem];
  [a nilInArrayLiteral0];
  [a nilInArrayWithObject];
  return 0;
}
