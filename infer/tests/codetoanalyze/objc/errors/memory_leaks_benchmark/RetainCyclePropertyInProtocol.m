/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#import <Foundation/NSObject.h>

@protocol Delegate
@property(nonatomic, weak, readwrite) id delegate;
@end

@interface MyCustomView : NSObject<Delegate>
- (instancetype)initWithDelegate:(id)delegate;
@end

@implementation MyCustomView
@synthesize delegate = _delegate;

- (instancetype)initWithDelegate:(id)delegate {
  _delegate = delegate;
  return self;
}

@end

@interface MyCustomViewController : NSObject
@property(nonatomic, strong, readwrite) id view;
@end

@implementation MyCustomViewController

- (void)loadView {
  MyCustomView* _myView = [[MyCustomView alloc] initWithDelegate:self];
  self.view = _myView;
}

@end

int main_good() {
  MyCustomViewController* controller = [MyCustomViewController new];
  [controller loadView];
  return 0;
}
