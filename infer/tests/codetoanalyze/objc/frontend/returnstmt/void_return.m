/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface MyClass : NSObject

@end

@implementation MyClass

- (void)aMethod {
  int i = 0;
  int j = 0;
  if (i == 0) {
    return;
  }

  if (j == 0) {
    i++;
  }
}

@end
