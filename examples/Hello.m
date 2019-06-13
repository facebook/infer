/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface Hello : NSObject
@property NSString* s;
@end

@implementation Hello
NSString* m() {
  Hello* hello = nil;
  return hello->_s;
}
@end
