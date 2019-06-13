/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
