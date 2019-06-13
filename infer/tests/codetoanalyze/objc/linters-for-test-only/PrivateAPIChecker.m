/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import "Foundation/NSObject.h"

@interface TestView : NSObject
@end

@implementation TestView

- (void)methodd {
  [self respondsToSelector:@selector(actionBla:)];
}

- (void)methoddd {
  [self respondsToSelector:@selector(actionButtonTapped:)];
}

@end
