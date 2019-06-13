/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

int g;

@interface My_manager : NSObject
- (int)m;

@end

@implementation My_manager

- (int)m {
  g = 7;
  void (^b)();
  int z = 3;
  b = ^() {
    g = z + 3;
  };
  b();
  int* p = 0;
  if (g == 6)
    return *p;
  else
    return z;
}
@end
