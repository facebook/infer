/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface PrematureNilTermA : NSObject {
}
@end

@implementation PrematureNilTermA

- (void)noProblem {
  NSArray* foo = [NSArray arrayWithObjects:@"aaa", @"bbb", nil];
}

- (void)nilInArrayWithObjects {
  NSString* str = nil;

  // nil argument in arrayWithObjects terminates the list of arguments
  // prematurely
  NSArray* foo = [NSArray arrayWithObjects:@"aaa", str, @"bbb", nil];
}

@end

int PrematureNilMain() {
  PrematureNilTermA* a = [PrematureNilTermA alloc];
  [a noProblem];
  [a nilInArrayWithObjects];
  return 0;
}
