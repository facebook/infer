/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface NSCAssertExample : NSObject
@end

@implementation NSCAssertExample {
 @public
  int _y;
}

@end

int assert_param_not_nil(NSCAssertExample* param) {
  NSCAssert(param != nil, @"Param should not be nil");
  return param->_y;
}

int call_param_not_nil_no_npe_good() { return assert_param_not_nil(nil); }

int deref_param(NSCAssertExample* param) { return param->_y; }

int call_deref_param_with_nil_npe_bad() { return deref_param(nil); }
