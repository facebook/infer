/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <UIKit/UIKit.h>

@interface AValue : NSObject
@end

@implementation AValue {
 @public
  int* n;
}

@end

@interface ViewController : UIViewController

@property(nonatomic, copy) AValue* a;

- (instancetype)initWithAValue:(AValue*)a;

@end

@implementation ViewController

- (instancetype)initWithAValue:(AValue*)a {
  if (self = [super initWithNibName:nil bundle:nil]) {
    _a = a;
  }
  return self;
}

- (void)viewDidLoad {
  self->_a = nil;
}

@end

@interface ViewController2 : UIViewController

@property(nonatomic, copy) AValue* a;

- (instancetype)initWithAValue:(AValue*)a;

@end

@implementation ViewController2

- (instancetype)initWithAValue:(AValue*)a {
  if (self = [super initWithNibName:nil bundle:nil]) {
    _a = a;
  }
  return self;
}

- (void)viewDidLoad {
  self->_a = nil;
}

- (void)viewDidDisappear:(BOOL)b {
  if (b) {
    self->_a = [AValue new];
  } else {
    [super viewDidDisappear:b];
  }
}

@end

int build_view_controller_bad() {
  AValue* a = [AValue new];
  ViewController* controller = [[ViewController alloc] initWithAValue:a];
  AValue* controller_a = controller.a;
  return controller_a->n;
}

int build_view_controller2_good() {
  AValue* a = [AValue new];
  ViewController2* controller = [[ViewController2 alloc] initWithAValue:a];
  AValue* controller_a = controller.a;
  return controller_a->n;
}
