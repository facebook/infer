/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#import <Foundation/Foundation.h>

@interface A : NSObject
@end

@implementation A

+ (void)foo {
  if (!(1)) {
    NSString* __assert_fn__ =
        [NSString stringWithUTF8String:__PRETTY_FUNCTION__];
    __assert_fn__ = __assert_fn__ ? __assert_fn__ : @"<Unknown Function>";
    NSString* __assert_file__ = [NSString stringWithUTF8String:"A.m"];
    __assert_file__ = __assert_file__ ? __assert_file__ : @"<Unknown File>";
    [[NSAssertionHandler currentHandler] handleFailureInFunction:__assert_fn__
                                                            file:__assert_file__
                                                      lineNumber:23
                                                     description:(@"Hello")];
  }
}

+ (void)bar {
  int phoneNumberError = 5;
  int PhoneNumberNoError = 10;
  if (!((phoneNumberError != PhoneNumberNoError))) {
    NSString* __assert_file__ = [NSString stringWithUTF8String:"A.m"];
    __assert_file__ = __assert_file__ ? __assert_file__ : @"<Unknown File>";
    [[NSAssertionHandler currentHandler]
        handleFailureInMethod:_cmd
                       object:self
                         file:__assert_file__
                   lineNumber:12
                  description:(@"Invalid parameter not satisfying: %@"),
                              @"phoneNumberError != PhoneNumberNoError"];
  }
}

@end
