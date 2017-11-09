/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#import <Foundation/NSObject.h>

@interface HappySadView : NSObject
@end

@implementation HappySadView

- (void)makeBadAction {
  [self copy];
}

- (void)foo {
}

@end

@interface SubClassTestClass : NSObject

@end

@interface HappySadView2 : SubClassTestClass
@end

@implementation HappySadView2

- (void)makeBadAction {
  [self copy];
}

- (void)foo {
}

@end
