/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
