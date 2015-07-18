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

-(void) nilInArrayLiteral {
  NSString *str = nil;

  // nil argument in array literal crashes
  NSArray *foo = @[@"aaa", str, @"bbb"];
}

@end

int main() {
  A *a = [A alloc];
  [a noProblem];
  [a nilInArrayLiteral];
  return 0;
}
