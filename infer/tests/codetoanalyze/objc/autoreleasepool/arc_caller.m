/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>
#import "no_arc_callee.h"

@interface ArcCaller : NSObject
@end

@implementation ArcCaller

- (void)callAllocObject_zero:(int)n {
  for (int i = 0; i < n; i++) {
    NoArcCallee* obj = [NoArcCallee allocObject];
  }
}

- (void)callNewObject_zero:(int)n {
  for (int i = 0; i < n; i++) {
    NoArcCallee* obj = [NoArcCallee newObject];
  }
}

- (void)callCopyObject_zero:(int)n x:(NoArcCallee*)x {
  for (int i = 0; i < n; i++) {
    NoArcCallee* obj = [NoArcCallee copyObject:x];
  }
}

- (void)callMutableCopyObject_zero:(int)n x:(NoArcCallee*)x {
  for (int i = 0; i < n; i++) {
    NoArcCallee* obj = [NoArcCallee mutableCopyObject:x];
  }
}

- (void)callGiveMeObject_linear:(int)n {
  for (int i = 0; i < n; i++) {
    NoArcCallee* obj = [NoArcCallee giveMeObject];
  }
}

- (void)callGiveMeObject_autoreleasepool_zero:(int)n {
  for (int i = 0; i < n; i++) {
    @autoreleasepool {
      NoArcCallee* obj = [NoArcCallee giveMeObject];
    }
  }
}

@end
