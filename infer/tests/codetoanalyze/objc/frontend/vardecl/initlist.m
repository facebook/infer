/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>
#import <UIKit/UIKit.h>

int main() {
  int z;
  int a[2][3] = {{z + 1, 2, 3}, {5, 6, 7}};
}

@interface C : NSObject

@end

int test() {
  C* c1 = [C alloc];
  C* c2 = [C alloc];
  C* a[3] = {[c1 init], c1, c2};
}

CGAffineTransform struct_init_test() {
  return (CGAffineTransform){.a = -1, .b = 0, .c = -0, .d = -1};
}
