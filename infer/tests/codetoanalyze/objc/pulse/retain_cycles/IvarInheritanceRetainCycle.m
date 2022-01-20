/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface IvarInheritanceSuperclass : NSObject {
 @public
  int x;
 @public
  IvarInheritanceSuperclass* a;
}

@end

@interface IvarInheritanceSubclass : IvarInheritanceSuperclass

@end

@implementation IvarInheritanceSubclass

@end

int field_superclass_main() {
  IvarInheritanceSubclass* b = [[IvarInheritanceSubclass alloc] init];
  b->x = 5;
  b->a = b; // create cycle --> leak
  return 0;
}
