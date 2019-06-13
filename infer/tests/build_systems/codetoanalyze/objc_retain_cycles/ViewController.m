/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import "ViewController.h"
#import "View.h"

@implementation ViewController {
  View* _hostingView;
}

- (instancetype)init {
  if (self = [super init]) {
    _hostingView = [[View alloc] init];
    _hostingView.delegate = self;
  }

  return self;
}

@end
