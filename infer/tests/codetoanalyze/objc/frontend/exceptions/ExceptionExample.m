/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

@interface ExceptionExample : NSObject

@end

@implementation ExceptionExample

- (void)test {
  @try {
    NSString* s = [NSString alloc];
  } @catch (NSException* exception) {
  } @finally {
    [self description];
  }
}

- (void)test1 {
  NSString* s = [NSString alloc];
  if (s) {
    @throw [NSException
        exceptionWithName:@"Something is not right exception"
                   reason:
                       @"Can't perform this operation because of this or that"
                 userInfo:nil];
  }
}

@end
