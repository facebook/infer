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
  NSArray *foo = [NSArray arrayWithObjects: @"aaa", @"bbb", nil];
}

-(void) nilInArrayWithObjects {
  NSString *str = nil;

  // nil argument in arrayWithObjects terminates the list of arguments prematurely
  NSArray *foo = [NSArray arrayWithObjects: @"aaa", str, @"bbb", nil];
}

@end

int main() {
  A *a = [A alloc];
  [a noProblem];
  [a nilInArrayWithObjects];
  return 0;
}
