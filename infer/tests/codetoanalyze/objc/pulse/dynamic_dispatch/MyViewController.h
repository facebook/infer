/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import "MainViewController.h"

#import <Foundation/NSObject.h>

@interface MyViewController : MainViewController
- (id)initWithNumber:(int)n;
@end

MyViewController* createMyViewController(int number);
