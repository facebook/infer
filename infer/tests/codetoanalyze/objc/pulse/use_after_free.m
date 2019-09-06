/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

@interface PulseTest : NSObject

- (int)use_after_free_simple_in_objc_method_bad:(int*)x;

@end

@implementation PulseTest

- (int)use_after_free_simple_in_objc_method_bad:(int*)x {
  free(x);
  return *x;
}

@end

int use_after_free_simple_bad(int* x) {
  free(x);
  return *x;
}
