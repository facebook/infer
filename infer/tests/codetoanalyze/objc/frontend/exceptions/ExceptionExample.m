/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
