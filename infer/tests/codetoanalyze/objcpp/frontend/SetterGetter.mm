/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

struct S {
  int f1;
  int f2;
};

@interface SetterGetter : NSObject

@property S x;

@end

@implementation SetterGetter

- (S)call_getter {
  return [self x];
}

- (void)call_setter:(S)param {
  [self setX:param];
}

@end
