/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <stdio.h>
#import "A.h"

bool predB() {
  A* a = [A new];
  // implOnlyFn added in A.m (is valid but missing from the tenv for B.m)
  [a implOnlyFn:1];
  // Missing_fld reported here causing the spec to be missing
  return (a.delegate ? 0 : 0);
}

// NULL_DEREFERENCE is reported on both branches since fB() lacks a spec
int badOnlyOneNDB() {
  int x, *p1 = 0, *p2 = 0;
  if (predB())
    x = *p1;
  else
    x = *p2;
  return x;
}
