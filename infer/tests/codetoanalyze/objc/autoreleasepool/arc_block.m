/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>
#import "no_arc_callee.h"

@interface ArcBlock : NSObject
@end

@implementation ArcBlock

+ (void)callIndexOfObjectPassingTest_linear_FN:(NSArray*)x {
  int i = [x indexOfObjectPassingTest:^BOOL(
                 NSObject* obj, NSUInteger idx, BOOL* stop) {
    NoArcCallee* o = [NoArcCallee giveMeObject];
    return false;
  }];
}

@end
