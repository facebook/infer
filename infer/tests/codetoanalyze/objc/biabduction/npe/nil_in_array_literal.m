/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface Arr : NSObject {
}
@end

@interface A : NSObject {
}
@end

bool myrand(void);

@implementation A
+ (instancetype)initA {
  if (myrand())
    return nil;
  else
    return [A new];
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

- (NSMutableArray*)insertNilBad {
  NSMutableArray* ar = [NSMutableArray new];
  A* a = [A initA];
  [ar addObject:@[ a ]];
  return ar;
}

@end

int ArrMain() {
  Arr* a = [Arr alloc];
  [a noProblem];
  [a nilInArrayLiteral0];
  [a nilInArrayWithObject];
  return 0;
}
