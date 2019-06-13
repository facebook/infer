/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import "A.h"

@implementation A
- (void)implOnlyFn:(int)v {
}
@end

bool predA() {
  A* a = [A new];
  [a implOnlyFn:1];
  return (a.delegate ? 0 : 0);
}

// NULL_DEREFERECE is (correctly) reported on only one of the branches
int badOnlyOneNDA() {
  int x, *p1 = 0, *p2 = 0;
  if (predA())
    x = *p1;
  else
    x = *p2;
  return x;
}
