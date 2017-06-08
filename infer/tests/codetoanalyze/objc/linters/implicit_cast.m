/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#import <Foundation/Foundation.h>

int func_with_uinteger_param(NSUInteger n) { return 0; }

int func_with_integer_param(NSInteger n) { return 0; }

void calling_funct_with_pointer() {
  NSString* s = @"Dulma";
  func_with_uinteger_param(s);
}

void calling_funct_with_pointer1() {
  NSString* s = @"Dulma";
  func_with_integer_param(s);
}

void integer_nil() { NSInteger n = nil; }

NSString* returns_s() { return @""; }

void calling_funct_with_pointer_from_a_method() {
  func_with_integer_param(returns_s());
}

@interface Implicit_cast : NSObject

@property NSString* p;

@end

extern NSString* const key;

@implementation Implicit_cast {

  NSDictionary<NSString*, NSNumber*>* _userInfo;
}

- (void)ivar_dictionary_item_call_funct_with_int {
  func_with_integer_param(_userInfo[key]);
}

- (void)property_with_int {
  func_with_integer_param(self.p);
}

@end
