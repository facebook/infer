/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
