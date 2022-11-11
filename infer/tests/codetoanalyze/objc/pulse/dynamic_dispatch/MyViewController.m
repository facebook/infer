/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import "MyViewController.h"

#import <Foundation/NSObject.h>

@implementation MyViewController
- (id)initWithNumber:(int)n {
  self.number = n;
  return self;
}

- (int*)getViewControllerNumber {
  int* p = NULL;
  return p;
}
@end

MyViewController* createMyViewController(int number) {
  return [[MyViewController alloc] initWithNumber:number];
}
