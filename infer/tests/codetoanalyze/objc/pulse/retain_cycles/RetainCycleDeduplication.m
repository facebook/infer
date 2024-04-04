/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

@interface CaptureController : NSObject

@property(nonatomic, strong, readwrite) id delegate;

@end

@implementation CaptureController

@end

@interface CViewController : NSObject

@property(nonatomic, strong) CaptureController* captureController;

@end

@implementation CViewController

- (void)setCaptureInteractionController_bad:
    (CaptureController*)captureController {
  if (_captureController != captureController) {
    _captureController.delegate = nil;
    _captureController = captureController;
    _captureController.delegate = self;
  }
}

@end

void test_setCaptureInteractionController_bad() {
  CViewController* a = [CViewController new];
  CaptureController* b = [CaptureController new];
  [a setCaptureInteractionController_bad:b];
}
