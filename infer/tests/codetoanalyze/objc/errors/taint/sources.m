/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>
#import <Foundation/NSHTTPCookie.h>

void testNSHTTPCookie1() {

  NSHTTPCookie* c = [NSHTTPCookie new];
  NSString* s = c.value;

  [NSString stringWithFormat:@"Test taint %@: ", s];
}

void testNSHTTPCookie2() {

  NSHTTPCookie* c = [NSHTTPCookie new];
  NSString* s = c.value;

  [NSString localizedStringWithFormat:@"Test taint %@: ", s];
}

void testNSHTTPCookie3() {

  NSHTTPCookie* c = [NSHTTPCookie new];
  NSString* s = c.value;

  [[NSString alloc] initWithFormat:@"Test taint %@", s];
}

void testNSHTTPCookie4() {

  NSHTTPCookie* c = [NSHTTPCookie new];
  NSString* s = c.value;

  [NSString stringWithString:s];
}
