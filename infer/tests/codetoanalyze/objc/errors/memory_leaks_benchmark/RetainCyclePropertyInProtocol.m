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
@property(nonatomic, strong, readwrite) id strong_delegate;
@end

@interface MyCustomView : NSObject<Delegate>
- (instancetype)initWithDelegate:(id)delegate;
@end

@implementation MyCustomView
@synthesize delegate = _delegate;
@synthesize strong_delegate = _strong_delegate;

- (instancetype)initWithDelegate:(id)delegate {
  _delegate = delegate;
  return self;
}

@end

@interface MyCustomViewController : NSObject
@property(nonatomic, strong, readwrite) id view;
@end

@implementation MyCustomViewController

- (void)loadViewGood {
  MyCustomView* _myView = [[MyCustomView alloc] initWithDelegate:self];
  self.view = _myView;
}

- (void)loadViewBad {
  MyCustomView* _myView = [[MyCustomView alloc] init];
  self.view = _myView;
  _myView.strong_delegate = self;
}

@end

int main_good() {
  MyCustomViewController* controller = [MyCustomViewController new];
  [controller loadViewGood];
  return 0;
}

int main_bad() {
  MyCustomViewController* controller = [MyCustomViewController new];
  [controller loadViewBad];
  return 0;
}
