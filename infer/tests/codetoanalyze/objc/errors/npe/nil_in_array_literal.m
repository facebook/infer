/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
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
