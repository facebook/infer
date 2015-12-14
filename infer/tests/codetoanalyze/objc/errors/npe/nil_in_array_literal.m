/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

@interface A : NSObject {

}
@end

@implementation A

-(void) noProblem {
  NSArray *foo = @[@"aaa", @"bbb"];
  // check that array literals create valid objects
  NSArray *foofoo = @[foo];
}

-(void) nilInArrayLiteral0 {
  NSString *str = nil;

  // nil argument in array literal crashes
  NSArray *foo = @[str];
}

-(void) nilInArrayLiteral1 {
  NSString *str = nil;

  // nil argument in array literal crashes
  NSArray *foo = @[str, @"bbb"];
}

-(void) nilInArrayLiteral2 {
  NSString *str = nil;

  // nil argument in array literal crashes
  NSArray *foo = @[@"aaa", str, @"bbb"];
}

-(void) nilInArrayLiteral3 {
  NSString *str = nil;

  // nil argument in array literal crashes
  NSArray *foo = @[@"aaa", @"bbb", str];
}

@end

int main() {
  A *a = [A alloc];
  [a noProblem];
  [a nilInArrayLiteral0];
  return 0;
}
