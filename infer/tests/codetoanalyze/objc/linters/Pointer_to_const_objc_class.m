/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

@interface C : NSObject

@property(strong) const C* ptr_to_const_objc_bad;

@property(strong) C* const const_ptr_to_objc_good;

@end
