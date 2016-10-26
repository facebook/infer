/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

@interface ADict : NSObject {
}
@end

@implementation ADict

- (void)noProblem {
  NSDictionary* foo = @{ @"aaa" : @"a value", @"bbb" : @"b value" };
  // check that dictionary literals create valid objects
  NSArray* foofoo = @[ foo ];
}

- (void)nilInDictionaryLiteralKey0 {
  NSString* str = nil;

  // nil argument in dictionary literal crashes
  NSDictionary* foo = @{ str : @"a value" };
}

- (void)nilInDictionaryLiteralValue0 {
  NSString* str = nil;

  // nil argument in dictionary literal crashes
  NSDictionary* foo = @{ @"aaa" : str };
}

- (void)nilInDictionaryLiteralKey1 {
  NSString* str = nil;

  // nil argument in dictionary literal crashes
  NSDictionary* foo = @{ str : @"a value", @"bbb" : @"b value" };
}

- (void)nilInDictionaryLiteralValue1 {
  NSString* str = nil;

  // nil argument in dictionary literal crashes
  NSDictionary* foo = @{ @"aaa" : str, @"bbb" : @"b value" };
}

- (void)nilInDictionaryLiteralKey2 {
  NSString* str = nil;

  // nil argument in dictionary literal crashes
  NSDictionary* foo = @{
    @"aaa" : @"a value",
    str : @"b value",
    @"ccc" : @"c value"
  };
}

- (void)nilInDictionaryLiteralValue2 {
  NSString* str = nil;

  // nil argument in dictionary literal crashes
  NSDictionary* foo = @{
    @"aaa" : @"a value",
    @"bbb" : str,
    @"ccc" : @"c value"
  };
}

- (void)nilInDictionaryLiteralKey3 {
  NSString* str = nil;

  // nil argument in dictionary literal crashes
  NSDictionary* foo = @{
    @"aaa" : @"a value",
    @"bbb" : @"b value",
    str : @"c value"
  };
}

- (void)nilInDictionaryLiteralValue3 {
  NSString* str = nil;

  // nil argument in dictionary literal crashes
  NSDictionary* foo = @{
    @"aaa" : @"a value",
    @"bbb" : @"b value",
    @"ccc" : str
  };
}

@end

int DictMain() {
  ADict* a = [ADict alloc];
  [a noProblem];
  [a nilInDictionaryLiteralKey0];
  return 0;
}
