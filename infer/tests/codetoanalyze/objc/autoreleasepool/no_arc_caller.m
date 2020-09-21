/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>
#import "arc_callee.h"

@interface NoArcCaller : NSObject
@end

@implementation NoArcCaller

- (void)callAllocObject_zero:(int)n {
  for (int i = 0; i < n; i++) {
    ArcCallee* obj = [ArcCallee allocObject];
  }
}

- (void)callNewObject_zero:(int)n {
  for (int i = 0; i < n; i++) {
    ArcCallee* obj = [ArcCallee newObject];
  }
}

- (void)callCopyObject_zero:(int)n x:(ArcCallee*)x {
  for (int i = 0; i < n; i++) {
    ArcCallee* obj = [ArcCallee copyObject:x];
  }
}

- (void)callMutableCopyObject_zero:(int)n x:(ArcCallee*)x {
  for (int i = 0; i < n; i++) {
    ArcCallee* obj = [ArcCallee mutableCopyObject:x];
  }
}

- (void)callGiveMeObject_linear:(int)n {
  for (int i = 0; i < n; i++) {
    ArcCallee* obj = [ArcCallee giveMeObject];
  }
}

- (void)callGiveMeObject_autoreleasepool_zero:(int)n {
  for (int i = 0; i < n; i++) {
    @autoreleasepool {
      ArcCallee* obj = [ArcCallee giveMeObject];
    }
  }
}

- (void)callGiveMeInt_zero {
  int i = [ArcCallee giveMeInt];
}

- (void)callGiveTaggedPointerString_zero {
  NSString* x = [ArcCallee giveTaggedPointerString];
}

- (void)callGiveTaggedPointerNumber_zero {
  NSNumber* x = [ArcCallee giveTaggedPointerNumber];
}

- (void)callGiveTaggedPointerIndexPath_zero {
  NSIndexPath* x = [ArcCallee giveTaggedPointerIndexPath];
}

- (void)callGiveTaggedPointerIndexSet_zero {
  NSIndexSet* x = [ArcCallee giveTaggedPointerIndexSet];
}

- (void)callGiveTaggedPointerDate_zero {
  NSDate* x = [ArcCallee giveTaggedPointerDate];
}

@end
