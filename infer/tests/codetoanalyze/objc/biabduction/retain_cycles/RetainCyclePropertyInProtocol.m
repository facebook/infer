/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

@protocol Delegate
@property(nonatomic, weak, readwrite) id delegate;
@property(nonatomic, strong, readwrite) id strong_delegate;
@end

@interface MyCustomView : NSObject<Delegate>
- (instancetype)initWithDelegate:(id)delegate;
- (instancetype)initWithStrongDelegate:(id)delegate;
@end

@implementation MyCustomView
@synthesize delegate = _delegate;
@synthesize strong_delegate = _strong_delegate;

- (instancetype)initWithDelegate:(id)delegate {
  _delegate = delegate;
  return self;
}

- (instancetype)initWithStrongDelegate:(id)strong_delegate {
  _strong_delegate = strong_delegate;
  return self;
}

@end

@interface MyCustomViewController : NSObject
@property(nonatomic, strong, readwrite) id view;
@end

@implementation MyCustomViewController

- (void)loadViewNoRetainCycleGood {
  MyCustomView* _myView = [[MyCustomView alloc] initWithDelegate:self];
  self.view = _myView;
}

- (void)loadViewRetainCycleBad {
  MyCustomView* _myView = [[MyCustomView alloc] initWithStrongDelegate:self];
  self.view = _myView;
}

@end
